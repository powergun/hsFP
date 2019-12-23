# Monad

## Monad morning haskell

source:

[Monday morning haskell](https://mmhaskell.com/monads/tutorial)

A Monad wraps a value or a computation with a particular context.
A monad must define both

- a means of wrapping normal values in the context (`return`)
- a way of combining computations within the context (`>>=`)

## A gentle introduction to haskell

source:

[A gentle introduction to monad](https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md)

given a monad type definition:

```haskell
data EitherIO e a = EitherIO {
    runEitherIO :: IO (Either e a)
}
```

What did we get just by doing this? Let's see:

```haskell
λ> :type EitherIO
EitherIO :: IO (Either e a) -> EitherIO e a

λ> :type runEitherIO
runEitherIO :: EitherIO e a -> IO (Either e a)
```

So already we have a way to **go between our own type and the combination**
we used previously! That's gotta be useful somehow.

### Either monad's early-termination

see src/Either/Bind.hs

this example documents the early-termination characteristic of Either
monad: if the computation runs inside Either monad, any Left value
will cause early-termination, without the caller doing any exception
handling at all

## "From Scratch"

inspired by:

<https://ocharles.org.uk/posts/2016-01-26-transformers-free-monads-mtl-laws.html>

> Building a concrete monad can also be a lot of work. Consider a
> computation that needs access to some local state, a fixed environment
> and arbitrary IO. This has a type such as

```haskell
newtype M a = M (Environment -> State -> IO (a, State))
```

### re-discover monad

See src/FromScratch/Monad.hs

this example documents some monad concepts I found difficult to
understand; they make sense to me now from the computation's point
of view (i.e. I know how to map these concepts in real problem solving)

I call this re-discovery.

- the monad type (Environment -> State -> (a, State)) defines the
  computation accepted;
- the implementation of its functor, applicative and monad instance
  must account for the above point and design the "3 intentions of monad"
  see haskellFoo/wk12 notes (dependency, collapse, compose)
- getState() crafts a function that pass the hardcoded state value
  to the rhs of the bind operator; similar mechanism applies to
  getEnvironment()
- getState(), getEnvironment() may have a different type signature
  than the main computation function (M State, M Environment, v.s.
  M Int) - this is the power of monad: the computation encapsulated
  can have any type
- similarly any sub-computation can return its own type of choice:
  M a, which seems a trivial concept to me now
- but beware that Environment, unlike State, is purely read-only;
  there is no way to update Environment because this value is not
  part of the return value feeding to the rhs of the bind operator!

this example also shows that (quoting the words from the article):

> To combat this, we can make use of monad transformers. Unlike monads,
> monad transformers compose, which means we can build larger monads by
> stacking a collection of monad transformers together. The above monad
> M can now be defined using off-the-shelf components, but crucially we
> can derive all the necessary type classes in one fell swoop with the
> `GeneralizedNewtypeDeriving` language extension

Note I didn't use the generalized-deriving approach, but a simpler
type-alias approach (there are many examples) - this way I can use
the monad transformer such as ReaderT, StateT without rolling my
own machinary

### re-discover mtl

> This suggests that these lifts could be inferred by the use of type
> classes, and this is the purpose of the monad transformer library – mtl.

## Validation (an upgraded Either)

see: <https://hackage.haskell.org/package/Validation>

inspired by: hsInterview/da

(this challenge contains a hand-rolled Validation type that uses a list
monad, `[String]`, instead of a generic monadic type)

see Validation.EnsureValueInList.hs for the prelimiary demo:

iterate over a list A, if element 'a is NOT a member of list B,
log an error; the resulting errors are collected in list C
