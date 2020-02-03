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

UPDATE: see the RE-rediscover section below - **it is not possible to
implement a monadic instance for the Validation type**

see: <https://hackage.haskell.org/package/either-5.0.1.1/docs/Data-Either-Validation.html#v:validationToEither>

inspired by: hsInterview/da

(this challenge contains a hand-rolled Validation type that uses a list
monad, `[String]`, instead of a generic monadic type)

see Validation.EnsureValueInList.hs for the prelimiary demo:

iterate over a list A, if element 'a is NOT a member of list B,
log an error; the resulting errors are collected in list C

see also: applicative/Validation for a case analysis of applicative
validation

### Thread Errors

Validation is useful in the case of "registration form" problem:

a user-submitted form may contain any number of errors; the system
should collect all the errors and report them

## Test Monad Law using QuickCheck (following applicative)

inspired by First Principles. see also applicative/test/DemoQuickCheckApplicativeLaw.hs

see: test/DemoQuickCheckMonadLaw.hs

the take-home notes are:

- as shown in P/786, [ ] (a, b, c) is the monad under test; while in my
  case Validation e (a, b, c) is the SUT; (a, b, c) is to satisfy the
  TestBatch's type requirement
- how to define Arbitrary instance for my custom types (Validation and DD)
- ditto for EqProp instance
- how to use `quickBatch` (use the bottom value hack to express the types)
- learn to use QuickCheck generator, such as `choose` and `oneof`

## RE-rediscover Monad: Monad as taught in The First Principles

source: first principles P/755;

> you can derive Applicative and Functor in terms of Monad, just as you can derive Functor in terms of Applicative.

### Monad is a generalization of concat

AKA. **1st Foundamental Purpose of Monad**

> Monad, in a sense, is a generalization of concat! The unique part of Monad is the following function:

```haskell
import Control.Monad (join)
join :: Monad m => m (m a) -> m a

λ> import Control.Monad (join)
λ> join $ fmap (\x -> [x, 1]) [1..10]
[1,1,2,1,3,1,4,1,5,1,6,1,7,1,8,1,9,1,10,1]

-- implement bind using the generalized concat
-- Write bind in terms of fmap and join
λ> bind f ma = join $ fmap f ma
λ> :t bind
bind :: Monad m => (a1 -> m a2) -> m a1 -> m a2
λ> bind (return . (+ 1)) [1..10]
[2,3,4,5,6,7,8,9,10,11]
λ> (flip (>>=)) (return . (+ 1)) [1..10]
[2,3,4,5,6,7,8,9,10,11]
```

### liftM is liftA; and liftM2 (when specialized to list) is zipWith

liftM is for historical backward compatibility (applicative came later
than monad); beware that liftM and liftA does have different type constraint.

> Well, the types are the same, but the behavior differs. The differing behavior has to do with which list monoid is being used.

```haskell
λ> import Data.List (zipWith)
λ> import Control.Monad (liftM, liftM2)
λ> import Control.Applicative (liftA, liftA2)
λ> liftM (+ 1) [1, 2]
[2,3]
λ> liftA (+ 1) [1, 2]
[2,3]
λ> liftM2 (+) [1, 2] [10, 20]
[11,21,12,22]
λ> zipWith (+) [1, 2] [10, 20]
[11,22]
λ> zipWith (,) [1, 2] [10, 20]
[(1,10),(2,20)]
```

### IO monad and effect-merging

source: P/767

> What `join` did here is merge the effects of `getLine` and `putStrLn` into a single IO action. This merged IO action performs the effects in the "order" determined by the nesting of the IO actions. As it happens, the cleanest way to express "ordering" in a lambda calculus without bolting on something unpleasant is through nesting of expressions or lambdas.
> Sometimes it is valuable to suspend or otherwise not perform an IO action until some determination is made, so types like IO (IO ()) aren’t necessarily invalid, but you should be aware of what’s needed to make this example work

```haskell
λ> getLine <$> putStrLn
λ> putStrLn <$> getLine
23
λ> join $ putStrLn <$> getLine
2341
2341
```

### List Monad

```haskell
λ> :t (['a'] :: [ ] Char)
(['a'] :: [ ] Char) :: [Char]
λ>
```

the bind operation binds individual values out of the list;
`>>= :: [a] -> (a -> [b]) -> [b]` same effect as `x <- xs`

### Monad to express dependency

AKA. **2nd Foundamental Purpose of Monad**

P/773

> With the Maybe Applicative, each Maybe computation fails or succeeds independently of each other. You’re just lifting functions that are also Just or Nothing over Maybe values. With the Maybe Monad, computations contributing to the final result can choose to return Nothing based on “previous” computations.

### Monad: fail fast, like an overfudded startup

P/779

> the bind function will drop the entire rest of the computation on the floor the moment any of the functions participating in the Maybe Monad actions produce a Nothing value

```haskell
λ> Nothing >>= undefined
Nothing
λ> Just 1 >>= undefined
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
  undefined, called at <interactive>:69:12 in interactive:Ghci26
λ>
```

> Note that Either always short-circuits on the first thing to have failed. It must because in the Monad, later values can depend on previous ones

MY NOTES: **this is important** - see hsInterview/da/validation for a its
impact to the real world solution

> So, there is no Monad for Validation. Applicative and Monad instances must have the same behavior. This is usually expressed in the form:

```haskell
import Control.Monad (ap)
(<*>) == ap
```

> The problem is you can’t make a Monad for Validation that accumulates the errors like the Applicative does. Instead, any Monad instance for Validation would be identical to the Either’s monad instance.

### Kleisli Composition

AKA. **3rd Foundamental Purpose of Monad**

> function composition written in terms of `>>=` to allow us to deal with the extra structure

see: src/FirstPrinciples/KleisliComposition.hs

### What Monad is Not (Haskell Wiki)

<https://wiki.haskell.org/What_a_Monad_is_not>

### Explain Join using IO

see: `io-sinbin`; emphasis on merging the nested effects
