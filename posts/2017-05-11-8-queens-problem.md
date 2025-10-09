---
title: 8 Queens Problem
description: |
  Solution to the 8 Queens Problem in Haskell's type system
  using type families and singletons.
tags: haskell
---

I heard of this problem recently, based on Aphyr's eccentrically written
[typing the technical interview](https://aphyr.com/posts/342-typing-the-technical-interview)
blog post. It's an interesting read, but sadly, I am not a witch,
so I found the functional dependency style used quite difficult
to understand. I therefore  thought I would spend a few minutes
bashing out a simple solution at the term level, then attempt to
port it to the type level using more modern Haskell techniques
with the `singletons` library and type families.

The question, which is apparently common in interviews, is to
discover ways of placing 8 queens on a chessboard without any of
them being able to attack one another.

Here's my take at the term level version: my solution is a monadic
fold using list's monad instance, where the accumulator is keeping
track of the Queens' positions in each row we have placed already.

```haskell
import Control.Monad

solution :: [[Int]]
solution = foldM place [] [1..8]

place :: [Int] -> a -> [[Int]]
place current _ =
  (: current) <$> filter (safe current) [1..8]

safe :: [Int] -> Int -> Bool
safe xs x =
  and
    [ all (/= x) xs
    , all (/= x) (zipWith (+) xs [1..])
    , all (/= x) (zipWith (-) xs [1..])
    ]
```

It's a simple solution. For the type level, I'll be doing it quite
differently to Aphyr.

```haskell
{-# LANGUAGE TypeOperators, DataKinds, PolyKinds, TypeFamilies, KindSignatures,
             TemplateHaskell, GADTs, UndecidableInstances, RankNTypes,
             ScopedTypeVariables, FlexibleContexts, TypeInType #-}

module Main where

import Data.Singletons
import Data.Singletons.Prelude
import Data.Singletons.Prelude.List
import Data.Singletons.TypeLits
```

With that out of the way I can define a type level `safe` function
which returns all the safe locations which a queen can go in a row,
given the positions of queens in the previous rows.

```haskell
type family Safe (b :: [Nat]) (a :: Nat) :: Bool where
  Safe xs x =
    And
      '[ All ((:/=$$) x) xs
       , All ((:/=$$) x)
         ( ZipWith (:+$) xs                   (EnumFromTo 1 8) )
       , All ((:/=$$) ( 10 :+ x ))
         ( ZipWith (:-$) (Map ((:+$$) 10) xs) (EnumFromTo 1 8) ) ]
```

I was tripped up a few times writing this function. The first thing
was just trying to find the right version of the equality functions
to call. The `$$` ending specifies the right number of partial
applications to apply, but it's a real pain to discover due to the
template haskell generated haddocks of the `singletons` library
being terrible. The next thing was the natural number subtraction.
My type families were getting stuck without the `+10` when a negative
number would have been produced, but it was quite hard to see what
the problem really was.

Below I define my partial application for `Safe`. Unfortunately,
just like Aphyr I still need to do this as Haskell type application
can't be partially applied. But using data kinds and type in type
makes this a lot safer here, with a proper type signature keeping
everything in place.

```haskell
data Safe1 :: [Nat] -> (Nat ~> Bool)
type instance Apply (Safe1 xs) x = Safe xs x
```

The `(~>)` and the `Apply` type family come from the `singletons`
library, and represent type level functions, and their application
respectively.  Next, we filter a candidate set of positions using
the safe function, with the help of some additional type families
from `Data.Singletons.Prelude.List`.

```haskell
type family Place (a :: [Nat]) (b :: k) :: [[Nat]] where
  Place xs ignore =
    Map (FlipCons1 xs) (Filter (Safe1 xs) (EnumFromTo 1 8))

data FlipCons1 :: [Nat] -> (Nat ~> [Nat])
type instance Apply (FlipCons1 xs) x = x ': xs

data Place1 :: [Nat] -> (ignore ~> [[Nat]])
type instance Apply (Place1 xs) b = Place xs b

data Place2 :: ([Nat] ~> ignore ~> [[Nat]])
type instance Apply (Place2) xs = Place1 xs
```

To write the FoldM we need to perform type application using
the `Apply` instances. This is actually pretty easy, and there
is even a `(@@)` operator to help us here. Again, we also need
to define a function for the partially applied recursive term.

```haskell
type family FoldM ( f :: b ~> a ~> [b] ) ( acc :: b ) ( over :: [a] ) :: [b] where
  FoldM f acc '[] = '[ acc ]
  FoldM f acc ( x ': xs) =
    ConcatMap (FoldM1 f xs) (f @@ acc @@ x )

data FoldM1 :: ( b ~> a ~> [b] ) -> [a] -> ( b ~> [b] )
type instance Apply (FoldM1 f xs ) acc = FoldM f acc xs
```

Next up is the solution. It's written almost identically to
the term level code above, with a fold over placements.

```haskell
type family Solutions :: [[Nat]] where
  Solutions = FoldM Place2 '[] (EnumFromTo 1 8)
```

It appears to work too

```haskell
*Main> :t Proxy :: Proxy Solutions
Proxy :: Proxy Solutions
  :: Proxy
       ('[4, 2, 7, 3, 6, 8, 5, 1]
        :$$$ '['[5, 2, 4, 7, 3, 8, 6, 1], '[3, 5, 2, 8, 6, 4, 7, 1],
        ...
```

Conclusions
-----------

Modern Haskell's type level code is actually pretty succinct, and
even has a good degree of type safety. Using less code, we are
pretty easily able to get all the solutions (instead of one in the
cited blog post), and my type errors when writing it actually
existed, which was a plus.

Bonus - as Template Haskell
---------------------------

Interestingly, a lot of what was written above can be generated for
us by the `singletons` library (indeed, this is how the `singletons`
library is written).

For example, the `FoldM` type family above can be completely replaced
with
```haskell
import Data.Singletons.TH
$(singletonsOnly [d|
  foldM :: (b -> a -> [b]) -> b -> [a] -> [b]
  foldM _ acc [] = [acc]
  foldM f acc (x : xs) =
    concatMap (\acc' -> foldM f acc' xs) (f acc x)
  |])
```
and the library will even generate for us the partially applied
terms.

One can actually get very close to the result with just the code
from my term level calculation in a `singletonsOnly` splice. It is
however a bit finicky, and often complains about functions not
existing when there is a type family for them, so it's a bit all
or nothing. Plus, one loses a lot of readability in their error
messages and the haddocks will be phenomenally bad.
