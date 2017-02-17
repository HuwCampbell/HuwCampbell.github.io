---
title: Grenade
description: An introduction to Grenade, deep learning in Haskell
tags: haskell
---

I recently gave a talk at FP-Syd on my neural network library [Grenade](https://github.com/HuwCampbell/grenade). In some
sense, I did this because on a previous evening there, after mentioning that I
was writing one, was told that "everybody has a neural network library". I felt
that I should present what makes Grenade different from other ANN libraries out
there.

Introduction
============

Grenade is a dependently typed, high level, artificial neural network library
written in Haskell. It's pretty robust, fast, and expressive, and uses some
interesting type level programming techniques which don't appear to have been
used in neural networks before.

I have found that its purely functional nature, and type level features, make
development and composition of neural networks fast and easy, indeed, I was
able to write my first [*generative adversarial network*](https://arxiv.org/abs/1406.2661)
based on high level descriptions in a few hours, using no other libraries for
reference.

As a very simple example, I was able to write a convolutional neural network
for the classic MNIST challenge in just a few lines of code with

```haskell
type MNIST =
  Network
    '[ Convolution 1 10 5 5 1 1, Pooling 2 2 2 2, Relu,
       Convolution 10 16 5 5 1 1, Pooling 2 2 2 2, Relu,
       Flatten, FullyConnected 256 80, Logit,
       FullyConnected 80 10, Logit ]
    '[ 'D2 28 28,
       'D3 24 24 10, 'D3 12 12 10, 'D3 12 12 10,
       'D3 8 8 16, 'D3 4 4 16, 'D3 4 4 16,
       'D1 256, 'D1 80, 'D1 80,
       'D1 10, 'D1 10 ]

randomMnist :: MonadRandom m => m MNIST
randomMnist = randomNetwork
```

`MNIST` is a type alias for our neural network. This type contains not only a
type level list of the layers of the network, but also type level list of the
shapes of data which are passed between these layers.

What's really interesting here is that the function to construct this network
with random weights is one function call to the function `randomNetwork`.
That is, our types are so rich that no specific term level code is required to
create this network.

Background
==========

Deep learning is a big field. Today speech is understood and generated with LSTM
networks, language is translated, images are classified, and videos captioned, and
faces are recognised.

The classic picture of a stack of fully connected neurons with a non-linear
activation function does not cut it these days, and simple networks using this
structure are actually probably better replaced with traditional machine learning
techniques such as gradient boosted trees, SVMs, or even linear learners.

Neural Networks today are composed of **Layers**, arranged into a directed graph.
Usually, these are acyclic graphs, but recurrent networks may indeed involve cyclic
components as well. It is the composition of these layers which give modern neural
nets their expressiveness.

The variety of layers is broad, and the Caffe libraries offers over 50 in its
zoo.

How are they trained?
---------------------

Many layers have learnable parameters. Convolutional layers for instance learn
kernel filters. They are trained by calculating the partial derivatives for the
output loss function for every component to train using reverse automatic
differentiation. An optimiser is used to assess how much to change the weights in
the direction of the gradient, and make the network more likely to give the correct
answer.

Automatic differentiation is really cool, but relatively unknown, it's

a) Not numeric differentiation
b) Not symbolic differentiation

It's its own beast, and works by carrying along partial derivatives at each stage
of a complex computation.

A forward mode implementation is relatively trivial, and has been described
[elsewhere](http://conway.rutgers.edu/%7Eccshan/wiki/blog/posts/Differentiation/)
but is simple enough to write here as well.

```haskell
module AD where

data D s a = D a a deriving (Eq , Show)

lift :: Num a => a -> D s a
lift x = D x 0

dependent :: Num a => a -> D s a
dependent x = D x 1

instance Ord a => Ord (D s a) where
  compare (D x _) (D y _) = compare x y

instance Num a => Num (D s a) where
  D x x' + D y y' = D (x + y) (x' + y')
  D x x' * D y y' = D (x * y) (x' * y + x * y')
  negate (D x x') = D (negate x) (negate x')
  abs    (D x x') = D (abs x) (signum x * x')
  signum (D x _)  = lift (signum x)
  fromInteger x   = lift (fromInteger x)

instance Floating a => Floating (D s a) where
  pi              = D pi 0
  exp    (D x x') = D (exp x) (exp x * x')
  log    (D x x') = D (log x) (x' / x)
  sin    (D x x') = D (sin x) (cos x * x')
  cos    (D x x') = D (cos x) (-sin x * x')
  asin   (D x x') = D (asin x) (x' / sqrt( 1- x^2))
  acos   (D x x') = D (acos x) (-x' / sqrt( 1- x^2))
  atan   (D x x') = D (atan x) (x' / (1 + x^2))
  sinh   (D x x') = D (sinh x) (x' * cosh x)
  cosh   (D x x') = D (cosh x) (-sinh x * x')
  asinh  (D x x') = D (asinh x) (x' / sqrt(1 + x^2))
  acosh  (D x x') = D (acosh x) (x' / (sqrt(x -1) * sqrt(x+1)))
  atanh  (D x x') = D (atanh x) (x' / (1 - x^2))

instance Fractional a => Fractional (D s a) where
  recip (D x x') = D (recip x) (-x'/x/x)
  fromRational x = lift (fromRational x)

-- Numerical differentiation (for tests)
numerical :: (Fractional a, Num a) => (a -> a) -> a -> a
numerical f x =
  let bigger  = f (x + 0.000005)
      smaller = f (x - 0.000005)
  in  (bigger - smaller) / 0.00001
```

The main data type `D s a = D a a` carries its value in the first position
and its gradient in the second position. The `s` type is a phantom type to
ensure we can't mix up gradients for different calculations, in a similar
way to how `runST` has a phantom type to ensure refs can't be mixed between
mutable calculations.

The idea is that we can create a variable with its derivative using
`dependent`. This variable has a derivative of 1, since, if we change the
value, the value will change by the same amount :). When we apply a function
to this variable, its new derivative is calculated as well.

What's nice about this, is that we can calculate the partial derivative
of any calculation in a single pass.

### Reverse mode AD

Reverse mode is a little bit more complex to write in Haskell, but is
also implement in the *ad* library.

The idea is to keep a list (called the Wengert Tape) of values entering
every step of the computaion, then, once the gradient we want to back
propagate is calculated, we can work backwards through the calculation,
calculating the new partial derivatives at every point.

Again, what's nice is that we can calculate the partial derivative for
every weight and learnable parameter in the system using a single fowards
and backwards pass. Whereas if we were to use numerical differentiation, we'd
need to run two calculations for every partial derivative we wanted to
calculate. As neural nets often have over a million learnable parameters
(with some well over a billion), this is an obvious advantage.

What makes a decent Neural Network library?
-------------------------------------------

It must be fast. Some models can take days or more to train, so efficiency is
important. In Haskell, this unfortunately means that the *ad* library can't be used
in anger, as it requires a `Functor` constraint, and can't therefore use *BLAS*
(there's an issue to fix this).

It must be compositional. Some of the bigger neural nets involve hundreds of layers
organised into large complex graphs.

Design of Grenade
=================

To understand Grenade's design, it pays to first read [Justin Le's](https://github.com/mstksg),
blog on [dependently typed fully connected networks](https://blog.jle.im/entry/practical-dependent-types-in-haskell-1.html),
provided many ideas for the type level ideas I needed. I would
also note that some of the types given here may not be exactly what
is in the library, as I've excluded a few constraints which don't
aid understanding.

Grenade's Neural networks pass multidimensional matricies between
layers. These matricies, need not be of the same dimensionality,
so we define a `Shape` type

```haskell
data Shape
  = D1 Nat
  | D2 Nat Nat
  | D3 Nat Nat Nat
```

We can make data of these shapes using GADTs and data kinds

```haskell
data S (n :: Shape) where
  S1D :: ( KnownNat o )
      => R o
      -> S ('D1 o)

  S2D :: ( KnownNat rows, KnownNat columns )
      => L rows columns
      -> S ('D2 rows columns)

  S3D :: ( KnownNat rows
         , KnownNat columns
         , KnownNat depth)
      => L (rows * depth) columns
      -> S ('D3 rows columns depth)
```

I have also written some singletons instances for these shapes,
which helps implement a good few functions, such as `fromIntegral`.
This helps up march up and down between terms, types and  kinds
and work at the type level.

I couldn't actually find any other examples of writing singletons
without template haskell before, so I'll print them here as an
example of what this looks like.

```haskell
data instance Sing (n :: Shape) where
  D1Sing :: KnownNat a => Sing ('D1 a)
  D2Sing :: (KnownNat a, KnownNat b) => Sing ('D2 a b)
  D3Sing :: (KnownNat a, KnownNat b, KnownNat c) => Sing ('D3 a b c)

instance KnownNat a => SingI ('D1 a) where
  sing = D1Sing
instance (KnownNat a, KnownNat b)
  => SingI ('D2 a b) where
    sing = D2Sing
instance (KnownNat a, KnownNat b, KnownNat c)
  => SingI ('D3 a b c) where
    sing = D3Sing
```

Next we define what a layer is in Grenade, there's actually two
associated classes.

```haskell
class UpdateLayer x where
  type Gradient x :: *
  runUpdate       :: LearningPamameters -> x -> Gradient x -> x
  createRandom    :: MonadRandom m => m x

class UpdateLayer x => Layer x (i :: Shape) (o :: Shape) where
  -- | The Wengert tape for this layer.
  type Tape x i o :: *

  -- | Take the input from the previous
  --   layer, and give the output from this layer.
  runForwards    :: x -> S i -> (Tape x i o, S o)

  -- | Back propagate a step, computing the gradients
  runBackwards   :: x -> Tape x i o -> S o
                 -> (Gradient x, S i)
```

The first class `UpdateLayer` defines what data structure we can
use to pass around the `Gradient` for this layer, as well as how
we can perform a step of stochastic gradient decent.

The second class `Layer` is used to define what shapes of data a
layer can transform between. There's no limit on how many
instances of `Layer` a single type can take, as there might be
shapes on which a layer can work.

The `Tape` type describes what data is needed to be able to
calculate the partial derivatives leading into the layer (and the
layer's gradients) given the partial derivatives of its output.

The reason for the two classes is simply that we don't want
to have to provide `Proxys` for the input and output shapes when
they aren't required, nor duplicate the `runUpdate` function and
friends when there is more than one set of shapes which can be
transformed.

Example Layers
--------------

As an example, here' the definition of the `Tanh` layer. This
is a relatively simple layer, which simply applies the non-linear
hyperbolic tangent function to activation coming into the layer.

```haskell
instance UpdateLayer Tanh where
  type Gradient Tanh = ()
  runUpdate _ Tanh () = Tanh
  createRandom = return Tanh

instance (a ~ b, SingI a) => Layer Tanh a b where
  type Tape Tanh a b = S a
  runForwards _ a = (a, tanh a)
  runBackwards _ a g = ((), tanh' a * g)
```

One can see that `type Gradient Tanh = ()` which means that there
are no learnable parameters, for this layer.

The single `Layer` instance, states that the layer can perform the
transformation of any input and output shapes, as long as they are
equal, be they 1, 2, or 3 dimensional.

A layer with slightly more interesting `Layer` instances is the
`Reshape` layer. One of these is

```haskell
instance
  (KnownNat a, KnownNat x, KnownNat y, a ~ (x * y))
  => Layer Reshape ('D2 x y) ('D1 a) where
  ...
```

which says that we can flatten a 2 dimensional image into a single
vector, but only if the total number of points is equal. This is one
of many instances of the `Reshape` layer.

Finally an example of a `Layer` with learnable parameters is the
`FullyConnected` layer.

```haskell

instance (KnownNat i, KnownNat o)
  => UpdateLayer (FullyConnected i o) where
  type Gradient (FullyConnected i o) = FullyConnected' i o
  ...
instance (KnownNat i, KnownNat o)
  => Layer (FullyConnected i o) ('D1 i) ('D1 o) where
  type Tape (FullyConnected i o) ('D1 i) ('D1 o) = S ('D1 i)
  ...
```

where the `FullyConnected'` type holds the weight matrix for the bias
and activation neurons.

Network
-------

With these building blocks, the definition of a `Network` is simple

```haskell
data Network :: [*] -> [Shape] -> * where
    NNil  :: SingI i => Network '[] '[i]

    (:~>) :: (SingI i, SingI h, Layer x i h)
          => !x -> !(Network xs (h ': hs))
          -> Network (x ': xs) (i ': h ': hs)
```

As well as some very similar heterogeneous like data types for
`Gradients` and `Tapes`, which are parameterised by the layers and
shapes in a similar way to the Network.

Three functions then provide all we really need to do machine learning

```haskell
-- | Running a network forwards with some input data.
--
--   This gives the output, and the Wengert tape required for back
--   propagation.
--
runNetwork :: forall layers shapes.
              Network layers shapes
           -> S (Head shapes)
           -> (Tapes layers shapes, S (Last shapes))

-- | Running a loss gradient back through the network.
--
--   This requires a Wengert tape, generated with the appropriate input
--   for the loss.
--
--   Gives the gradients for the layer, and the gradient across the
--   input (which may not be required).
--
runGradient :: forall layers shapes.
               Network layers shapes
            -> Tapes layers shapes
            -> S (Last shapes)
            -> (Gradients layers, S (Head shapes))

-- | Apply one step of stochastic gradient decent across the network.
applyUpdate :: LearningParameters
            -> Network layers shapes
            -> Gradients layers
            -> Network layers shapes
```

what's really nice about these functions is that they directly mirror
the functions in Layers, in fact, this match works so well that we can
embed a Network as a Layer in another Network

```haskell
instance UpdateLayer (Network sublayers subshapes) where
  type Gradient (Network sublayers subshapes) = Gradients sublayers
  runUpdate    = applyUpdate
  createRandom = randomNetwork

instance ( i ~ (Head subshapes)
         , o ~ (Last subshapes)
         ) => Layer (Network sublayers subshapes) i o where
  type Tape (Network sublayers subshapes) i o = Tapes sublayers subshapes
  runForwards  = runNetwork
  runBackwards = runGradient
```

Combined with a `Concat` layer, which runs two layers in parallel and
concatenates their output, we can now recreate directed acyclic graphs of
layers in Grenade in a composable manner.

As an example, we can train two parallel convolutional Networks
for MNIST using different kernel sizes, and combine their output
before running the fully connected layers.

```haskell
type MNIST5x5 =
  Network
    '[ Convolution 1 10 5 5 1 1, Pooling 2 2 2 2, Relu
     , Convolution 10 16 5 5 1 1, Pooling 2 2 2 2, Relu
     , FlattenLayer ]
    '[ 'D2 28 28, 'D3 24 24 10, 'D3 12 12 10, 'D3 12 12 10
     , 'D3 8 8 16, 'D3 4 4 16, 'D3 4 4 16, 'D1 256 ]

type MNIST7x7 =
  Network
    '[ Convolution 1 10 7 7 1 1, Pooling 2 2 2 2, Relu
     , Convolution 10 16 4 4 1 1, Pooling 2 2 2 2, Relu
     , FlattenLayer ]
    '[ 'D2 28 28, 'D3 22 22 10, 'D3 11 11 10, 'D3 11 11 10
     , 'D3 8 8 16, 'D3 4 4 16, 'D3 4 4 16, 'D1 256 ]

type MNIST  =
  Network
    '[ Concat ('D1 256) MNIST5x5 ('D1 256) MNIST7x7, FlattenLayer
     , FullyConnected 512 80, Logit, FullyConnected 80 10
     , Logit ]
    '[ 'D2 28 28, 'D2 2 256, 'D1 512, 'D1 80, 'D1 80
     , 'D1 10, 'D1 10 ]
```

We can also write an Inception network, which can be
embedded into a Network as a Layer easily.

Conclusions
-----------

Grenade does a lot of things right, it's very expressive in my
view, and makes the construction of networks incredibly safe:
they essentially can't crash, and they'll always be sound. It's
also pretty quick, using BLAS and some hand written C, I've found
this acceptable for a lot of problems.

The biggest downside is related to speed however, Grenade is CPU
only, and at this stage, does not use minibatching. I don't think
this is a big deal for a lot of architectures, but for some (LSTM)
it does come at a cost.

The next big downside is related to optimisation algorithms. At the
moment, I just use stochastic gradient decent with momentum, and
this is baked into the structure of each layer a bit too much. It's
totally reasonable to ask for a neural network library to permit
the ADAM optimiser, or something else.

There are actually haskell versions of a lot of these algorithms
already, but they require a `Functor` constraint on the types, which
I don't think I can do while maintaining the performance
characteristics of the library.

I think there is a decent solution, where the `Gradient` type
becomes the weights for a `Layer`, with a dictionary providing just
enough operations on it for the optimiser to be able to decide what
extra information it wants to hold onto (frequency of updates, last
update... etc).
