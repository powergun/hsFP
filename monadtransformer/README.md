# Monad Transformer

source: <https://en.wikibooks.org/wiki/Haskell/Monad_transformers>

haskell cookbook (web version) chapter 18

## Terminology

we will use **precursor monad** to refer to the non-transformer (e.g.
Maybe in MaybeT stack), on which a transformer is based;

**base monad** is the other monad (e.g. IO in MaybeT IO stack)
on which the transformer is applied

Since the `bind operator >>=` and `return` for the transformer
mirror the semantics of the precursor monad, a `do block` of type
`ReaderT Env IO String` will, from the outside, look a lot like a
do block of the Reader monad, except that `IO` actions become
trivial to embed by using lift

In general, there is no magic formula to create a transformer
version of a monad; the form of each transformer depends on what
makes sens in the context of its non-transformer type

## Lifting

`liftM`, applies a function (a -> b) to a value within a monad m,
we can also look at it as a **function of just one argument**

`liftM :: Monad m => (a -> b) -> (m a -> m b)`

liftM converts a plain function into one that acts within m

allows us to apply a plain function to a monadic value

lift function plays an analogous role when working with monad
transformers. It brings (aka promotes) base monad computations
to the combined monad. By doing so, it allows us to easily insert
base monad computations as part of a large computation in the
combined monad.

## Simple Transformer

to implement a StateT from scratch

[SimpleTransformer (haskell cookbook)](src/SimpleTransformer)

## Directory Walker

progressively improve a naive, non-Mt implementation to use Mt

[Directory walker (real world haskell)](src/DirWalker)

## Transformer Stack

the concept of using multiple Mt

[TransformerStack: tooling for the stack](src/TransformerStack)

## MaybeT

develop MaybeT from scratch and see how it dramatically simplifies
real-world use case that relies on early termination - the passphrase problem.

[Transformer from scratch: The Maybe Transformer](src/MaybeTrans)

## FoldStopEarly

source: <https://tech.fpcomplete.com/haskell/tutorial/monad-transformers>

a fold (left-fold) that does early-termination

following the article, this example explores the use of Either
and State monad; then shows how to use ExceptT to gracefully
handle effect.

recap on `bool`, `either` and `throwError`

## mtl style typeclasses

source: <https://tech.fpcomplete.com/haskell/tutorial/monad-transformers>

we've established that not only can the `State` monad itself perform
put and get actions, but any transformer layered on top of it can do
so as well.

mtl has a philosophy around generalizing this idea using typeclasses

## Rediscover Type-Composition, First Principles P/982

P/983

> (on `IdentityT/Identity`'s runIdentity function) these accessor functions
> are means of extracting the underlying value from the type.
> there is no real difference in meaning between `run` and `get`

P/984

> the important thing is that monad transformers are never sum or product
> types; they are always just a means of wrapping one extra layer of
> (monadic) structure around the type, so there is never a reason they
> couldn't be newtypes

see: src/FirstPrinciples/TypeComposition.hs for a mental exercise

P/986

> (on the Functor instance of `Compose`) now the `f, g` both have to
> be part of the structure that we are lifting over, so they both
> have to be Functors themselves

P/987

> the composition of two datatypes that have a functor instance gives
> rise to a new functor instance.
> you will sometimes see people refer to this as **functors being "closed under composition"**

### Functor, Applicative Instances of Type-Composition

see: <https://wiki.haskell.org/Type_composition> for a reference impl of
the Applicative instance of `Compose f g a` - note the use of `liftA2`;

the goal is to lift `<*>` to `fga` (over the Structure of Compose), and
leave the rest to be handled by the existing Applicative instance machinary -
because fga (i.e. `f and g`) is Applicative;

this is my ghci experiment (I was close to that reference impl):

```haskell
(liftA2 . liftA2 $ (<*>)) [Just [toUpper]] [Just ['c']]
[Just "C"]
```

### Compose Monad instances and issues

P/988

> composing Maybe and List... the result of having done so does not
> give you a Monad
> the issues comes down to a lack of information;
> you are trying to combine two polymorphic binds into a single combined bind
> and this is not possible
