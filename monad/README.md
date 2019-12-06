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
