---
title: Destructive Updates - a Stitch in Time
date: 2025-02-01
description: How the Tardis Monad and a Stitching Graph helps discover affine array usage, reducing run times by up to a half.
author: Huw Campbell
---

<h5>
How the Tardis Monad and a Stitching Graph helps discover affine array usage, permitting destructive updates.
</h5>

Icicle is a high-level streaming query language, which gives new capabilities to its users,
allowing them to combine and fuse hundreds of rich, individual, queries into a combined plan for
safe and efficient execution.

At the heart of the language is our Core intermediate language, which we've spoken about optimising
in [an earlier post](/posts/2020-09-04-traversals-for-optimisations.html). This is a pure,
simply-typed lambda calculus based DSL with restrictions on arbitrary recursion and closure creation.

To ensure speed, we compile all queries to C and convert almost all of our data types into simple,
unboxed C types. For example, the `Either Int Bool` type will compile into three C variables on the
stack, indicating the tag, and variables for the left and right values.

Maps and arrays in the language, however, compile using the struct of arrays approach when transitioning
to C; compiling down to potentially many C arrays of simple types. However, as Icicle Core is a _pure_
language, during lowering the compiler inserts copy operations to arrays before performing any
mutations upon them.

This is the story of how we eliminate the overwhelming majority of these copy operations from idiomatic
Icicle code by performing destructive updates during operations such as inserting into a map or sorting
an array, while maintaining query semantics and reducing run-times by up to 50%.

## Motivating Queries

It's very common in Icicle queries to use the built in `group` context, which acts somewhat like an
SQL group by. For example, the following query looks at a stream of injuries, then computes
a map of locations to the sum of the injuries' severities.

```elm
from injury in
  group location in sum severity
```

in our core language, this will become something like this (omitting error handling):

```elm
STREAM_FOLD accum : Map String Double
  INIT:
    Map []
  KONS:
    let
      severity =
        get_severity# input

      location =
        get_location# input

    in
      Map_insertOrUpdate#
        (\current -> add# current severity)
        severity
        location
        accum
```


When compiled to our lower level DSLs, and finally on to C, this query will end up being backed by
two arrays. As new events come in and are updated, it would be best to keep memory usage linear
with number of keys. Due to the implicit copy operation from the pure Core DSL though, a naïve
approach might end up using memory proportional to the product of the number of keys times and
number of injuries.


## A Retrospective - How Reference Counting Enables Copy Elision

The R programming language is popular amongst statisticians and data scientists, and has a number
of distinctive properties. Most operations in R are pure and referentially transparent, which is
critical for its ease of use and safety. It's also a language with pervasive vectorisation. All the
core data structures of R are vectors. With atomic vectors neatly wrapping C arrays.

People still want their R code to be relatively performant however, and adding naïve copying to
every vector update would be prohibitively slow.

Look at this R program:

```R
> x    <- 1:10
> y    <- x
> x[5] <- 10
> x
 [1]  1  2  3  4 10  6  7  8  9 10
> y
 [1]  1  2  3  4  5  6  7  8  9 10
```

This program maintains R's referential semantics, but under the covers there are some
interesting things going on. One might first assume that when
`y` is bound to `x`, a copy of the vector is made, but that is _not_ what R is going to do here.

In R, names are just ways to reference values, and in this case, there will be two names pointing
to the atomic vector `1:10`; and when `x` is going to be mutated, a copy of the backed array will
be made at that point in time.

This program however will do something very different:

```R
> x    <- 1:10
> x[5] <- 10
> x
 [1]  1  2  3  4 10  6  7  8  9 10
```

In this program, because there's only one name pointing to the vector in question, the value
will be updated in place. R can perform this optimisation because it's a reference counted
language and when the `[]<-` function is called, it can look at the number of references which
exist – then, if there's only one (itself), it knows it can modify in place without the
optimisation breaking the program's semantics (see [Advanced R]).

Other languages which employ this technique include the [Lean proof assistant][Counting Immutable Beans]
and functional programming language and [Koka][Precise, Automatic, Reference Counting], which
reuses constructors (like list's cons) too.


## Icicle – Compilation Time Copy Elision

In Icicle, we avoid using reference counting and instead use a simple bump allocator per
entity. We do this because:

- Except for arrays and strings all bindings are placed directly onto the stack;
- It's fast;
- Each entity runs separately and is bounded by the number of events; and
- It makes clearing memory when we've finished processing an entity close to trivial.

That said, we should still aim to reduce new array allocations though for speed and memory
efficiency, but here we do it entirely at compile time.

The constraints for this optimization are:

- It should remove copies if the original array will not be accessed again.
- It must not alter the results of a query.
- It must not unduly slow down compilation.

To eliminate a copy operation, we need to identify two key factors:

- All references that _might_ point to the array at the time of copying.
- All references that are _used_ after the copy is made.

If there are no subsequent usages of any reference that might point to the array about
to be copied, we can eliminate the copy operation and simply alias the binding instead.

But this presents a challenge: all the references we need to consider arise from before
the copy operation; while all the usages come from after it. So at any point in the
query, we need to have performed both forward and backward passes over the AST to
know what to do.

Finally, as Icicle's semantics are essentially a strict left fold, which is
compiled to a for loop, our internal DSL must contain looping structures (for and while).
One can imagine that even the definition of future and past accesses could get a little
muddy, as each loop could reference data from previous iterations.


## Avalanche - Our Internal DSL

We perform copy elision within a low level internal DSL called avalanche. This is a small
imperative language which contains basic _if_ and _foreach_ statements; and can create,
read from, and update mutable variables. It does _not_ perform IO.

As a Haskell type, the language looks a bit like this:

```haskell
data Statement
 -- | A branch
 = If Exp Statement Statement
 -- | Loop while an accumulator reads true.
 | While Name Statement
 -- | Loop over facts
 | ForeachFacts Statement
 -- | Execute several statements in a block.
 | Block [Statement]
 -- | Local binding.
 | Let Name Exp Statement
 -- | Initialise an accumulator
 | InitAccumulator !Name !Exp Statement
 -- | Read from an accumulator into a local binding
 | Read Name Name Statement
 -- | Update an accumulator
 | Write Name Exp
```

Where `Exp` is a simple expression type containing variables (`Var`), primitives,
and fully applied applications.

It's important to note that `Let`, `InitAccumulator` and `Read` introduce scope...
they contain the `Statement` that can use the binding introduced. To make life
easier, we disallow shadowing in this DSL – shadowed bindings in the source
language will be freshened.

Furthermore, we should note that Avalanche programs can get pretty large, with map
operations for example often including code for a binary search. Typical Avalanche
programs also contain the code for up to a hundred user queries.


## Building a Reference Graph

We want to build a directed graph of all references, so that we can determine if
a reference might be used again later in the program.

As this is Haskell, we use a pure, persistent data structure (here, backed by
a pair of `Data.Map Name (Set Name)`) which indicate forwards and backwards edges.

We maintain a few invariants:

- The `Set` must be non-empty, if a `Set` becomes empty due to a deletion, we
  also remove the key.
- If we delete a key from the graph, we stitch up its incoming and outgoing edges.
  So the graph `a -> b -> c` will become `a -> c` if `b` is removed.
  - This is why we refer to it as a Stitching Graph.
- If we `overwrite` an edge (this is our main `insert` operation), that edge
  becomes the only edge from the
  starting key. i.e., `overwrite a c (overwrite a b empty) == overwrite a c empty`.
- If we have a cycle, like `a -> b -> a` and remove `b`, the stitching to `a -> a`
  will be simplified to nothing, and the node will be removed.

And its API looks a bit like this:

```haskell
module Graph where

data Graph

-- | Insert an edge into a graph and remove existing ones
--   from this node.
overwrite :: Name -> Name -> Graph -> Graph

-- | Delete a node and stitch up transiently connected nodes
deleteAndStitch :: Name -> Graph -> Graph

-- | Take the union of the maps
merge :: Graph -> Graph -> Graph
```



Because this structure shares well, it's ok to build and retain this type as we
traverse through our program's AST. Using the `State` monad in this initial example:


```haskell
go :: Statement -> State Graph Statement
go statement =
  case statement of
    --
    -- Let bindings are a lot like reads and
    -- initialisations if the expression is
    -- a reference, then add to the graph.
    Let nm x inner
      | Just ref <- arrayReference x ->
        Let nm x <$>
          scopedGo nm ref inner

      | otherwise ->
        Let nm x <$>
          scopedGo' nm inner

    --
    -- If the write is a reference, then we need
    -- to know that this memory location has been
    -- updated.
    Write n x
      | Just ref <- arrayReference x -> do
        modify $
          Graph.overwrite n ref

        pure $ Write n x

      | otherwise ->
        pure $ Write n x

    --
    -- Traverse the block items in order.
    Block inner ->
      Block <$> traverse go inner

    --
    -- Run both sides and merge
    If condition truth falsity -> do
      aliased      <- get
      let ~(truth',   tA) = runState (go truth) aliased
      let ~(falsity', fA) = runState (go falsity) aliased

      modify $
        Graph.merge tA . Graph.merge fA

      return $
        If condition truth' falsity'

    --
    -- Reach a fixpoint, running until the
    -- graph doesn't change anymore.
    ForeachFacts inner -> do
      ForeachFacts <$>
        fixGo inner


 where
    --
    -- This introduces the alias for name to ref, then when it goes
    -- out of scope, deletes this name and stitches up transient refs
    -- as direct ones.
    scopedGo nm ref ss = do
      modify (Graph.overwrite nm ref)
      sS <- go ss
      modify (Graph.deleteAndStitch nm)
      pure sS

    --
    -- Delete this name after it goes out of scope
    -- (things might reference it).
    scopedGo' nm ss =
      sS <- go ss
      modify (Graph.deleteAndStitch nm)
      pure sS


    --
    -- Loops are interesting.
    -- It's possible to write queries where after a pass is complete
    -- accumulators depend on other accumulators initialised before
    -- the loop began; so we need to reach a fixpoint on both the
    -- initial alias map and the returned alias map.
    --
    -- If the maps don't match, rerun with the merged alias map on
    -- the original statements.
    fixGo ss = do
      before <- get
      sS     <- go ss
      after  <- get
      let merged = Graph.merge before after
      if before == merged then
        pure sS
      else do
        set merged
        fixGo ss


    --
    -- Whether this is a `Var` or a function which mutably
    -- updates an array and returns it.
    arrayReference :: Exp -> Maybe Name
    arrayReference = \case
      Var nm ->
        Just nm
      Exp.App (Exp.Prim Prim.SortInPlace) argument ->
        arrayReference argument
      ...
```

with `Read` and `InitAccumulator` omitted as they look very much like `Let`; and
`While` as it looks like `ForeachFacts`.


## Stepping into the Tardis

That's our forwards pass done, so at any point we know what the graph of
references looks like; but we're only half way there, as we don't know what
bindings are used later in the program.

For this we need to propagate information backwards, which we can do thanks
to laziness and the reverse state monad. One package providing a nice
combination of both a forwards and reverse state Monads is the [`tardis`][Tardis]
package, which supplies the `Tardis` monad.

In the above code listing, we swap to the `Tardis` monad and replace `get`
with `getPast` and `modify` with `modifyForwards`. Also, every time we see
a `Name` in either a `Var` in `Exp` or as the named item in, for example,
a `Read`, we send backwards the usages (free variables)

```haskell
go :: Statement -> Tardis (Set Name) Graph Statement
go statement =
  case statement of
    Let nm x ss
      | Just ref <- arrayReference x ->
        modifyBackwards (Set.delete nm . Set.union (Exp.freeVars x))
        Let nm x <$>
          scopedGo nm ref ss

      | otherwise ->
        modifyBackwards (Set.delete nm . Set.union (Exp.freeVars x))
        Let nm x <$>
          scopedGo' nm ss
```

Because we also remove bindings created by `Let` and friends as we
traverse backwards, effectively we're maintaining the `Set` of free variables
observed in this backwards pass.

## Performing the Copy Elision

Our elaborator to `Avalanche` will emit a copy operation before performing
any mutating action (for example: heap sort, or update), but fortunately
it emits these in a very predictable fashion – directly as the expression
being written to an accumulator. As such, we don't need to be too careful
threading the `Tardis` states through the `Exp` syntax tree in addition to
the `Statement` syntax tree and we can cheat a little and add a rewrite
directly in the definition of `go`

```haskell
go statements =
  case statements of
    --
    -- Here's the key judgement and rewrite.
    -- This write copies an array into an accumulator.
    Write n x
      | Exp.App (Exp.Prim Prim.ArrayCopy) (Exp.Var ref) <- x -> do
        modifyBackwards (Set.insert ref)
        aliased <- getPast
        used    <- getFuture

        --
        -- It's important to add this ref when we're calculating
        -- loop fixpoints.
        modifyForwards $
          Graph.overwrite n ref

        pure $
          --
          -- Here's the important calculation.
          -- Fortunately this is lazy, allowing us to "fix"
          -- the Tardis.
          if hasNoFurtherReferences n ref aliased used then do
            Write n (Exp.Var ref)
          else do
            Write n x -- keep the original
```


where the function `hasNoFurtherReferences` looks at the graph and
does a search from all used variables to all references which might be shared with them,
as well as a similar search from the ref. If these transitive dependency sets
are disjoint, it means there's no possible way that the array could be looked at
after this write, making it safe to mutate it in place.


If there was an intersection, that would indicated that a value might be used which
points to the same memory location as `ref`, and we have to keep the copy
operation to maintain our program's semantics.


## Fixing Faster with Stitches

One of the things which is challenging is the definition of `fixGo`. Because
we have to deal with nested loops quite a lot, it's imperative that most loops
reach their fixpoint on their first iteration, lest we suffer from
exponential slowdowns as we delve deeper repeating nested loops.

This is one the key reason for using our Stitching Graph – it's very good at returning
to its initial state when keys are removed as they go out of scope.

Consider this program, written in an pseudo-syntax for avalanche.

```elm
init
  arr =
    []
in {
  foreach_fact {
    read
      a =
        arr {- read `arr` to a local binding -}
    in {
      let
        b =
          array_copy a
        len =
          array_length b
      in {
        arr := {- write `len` to index `len` -}
          array_insert b len len
      }      
    }
  }  
}
```

This program should produce an array that looks like this:

```
[0,1,2,3,...]
```

with a length equal to the number of facts seen, but will
the copy operation be removed, and, will the fixpoint for _foreach_fact_
be determined in a single pass? Let's have a look:

1. The initialisation of `arr` is a pure expression, so doesn't introduce anything
   into the reference graph.
2. When we reach `foreach_facts` the first time, the graph is empty.
3. When we `read a = arr` the accumulator `arr` into `a`, the graph gains a reference
   - `a -> arr`
4. In let binding `b`, we reach a copy operation.
   - It's clear that no references of `arr` are used again (writing to it doesn't
     count as usage), so this copy will be omitted.
   - We have to add the reference `b -> a`
   - Our graph is now `b -> a -> arr`
5. Binding `len` doesn't introduce any references into the graph.
6. Writing to `arr` introduces a new edge to the graph, so we have
   - `arr -> b -> a -> arr`
7. As we leave the scope of the let bindings
   - b will be removed and we'll have `arr -> a -> arr`
8. As we leave the scope of the `read`, we'll further drop `a` from the graph
   - Here though, we would have `arr -> arr`, but, these are simplified,
   - Therefore our final graph after leaving the scope of `read` is the empty graph
9. As this was the graph at the start of the `foreach_facts`, we've reached our
   fixpoint in a single traversal of the AST, and the copy has been removed.

So the answer to both of our questions is in the affirmative.

## Commentary and Future Work

It's worth thinking about generalising this algorithm, and considering how it
relates to others previously described.

In a real sense, we've written an abstract interpreter for our programs, tracking possible
pointer references. While I am not 100% across the literature on reference
counting optimisations, this sort of information strikes me as potentially
very useful for optimising functional languages which use it.

For example, if we examine our reference graph and see that the potential references
will always be zero or no usages will be used in the future, we won't even need to check
the reference count at all and can either drop or recycle a memory location immediately.

We could further extend our Graph algorithm to include the possible range of counts between
references – which would mostly entail changes to the merging and stitching algorithms.
With this information, if we observed an object used inside a scoped operation
which always had the exact same positive number of edges to it at entry and exit,
it would seem we could again eliminate all reference counting operations on it –
they must cancel out.

In our case, we have a whole-program compiler and optimiser, so we can get a _very_ good
view of the reference graph; if we however were working with smaller functions, we would
need to keep track of what we don't know. Arguments passed to functions for example,
would have any number of possible edges from the outside of the function, meaning we can't
omit operations as easily. But as we inlined functions, new opportunities might arise.

For example, consider the function `map h . filter g . map f`: if this were to be aggressively
inlined, then we would _know_, that the `cons` cells output from `map f` part would have no
external references to them; this means that `filter g` could drop `cons` cells
without even looking at their reference count when the predicate fails. Furthermore,
the function `map h` could immediately reuse the memory locations for every cons cell still
around, replacing the values after applying `h` to them.

How much of an improvement if any this might offer over [Counting Immutable Beans] is an
open question.

## Application in a Strict Language

The discovery of this algorithm was challenging, it took time and iteration – and
even though I could intuitively see copy operations which could be removed,
writing an even close to `O(n)` algorithm was proving to be trifficult.

It was only when I remembered Phil Freeman's Tardis Monad [water problem solution][PAF Water] that
I managed to solidify my ideas and really nail down the algorithm.

*Laziness, purity, and monadic composition were critical in allowing me to discover this algorithm.*

That said, I think one could make this work in a strict language, by adopting a relatively
straight forward, explicit two pass approach, where in the forwards pass, instead of returning
a `Statement` value, we return a function, `Usage -> (Statement, Usage)`; and pass the usage
set backwards while building the graph in our reverse pass.
  
  [Advanced R]: http://adv-r.had.co.nz/memory.html#modification
  [Precise, Automatic, Reference Counting]: https://koka-lang.github.io/koka/doc/book.html#why-perceus
  [Counting Immutable Beans]: https://arxiv.org/pdf/1908.05647
  [PAF Water]: https://gist.github.com/paf31/9d84ecf6a6a9b69cdb597a390f25764d
  [Tardis]: https://hackage.haskell.org/package/tardis
