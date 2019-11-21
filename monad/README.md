# Monad

source:

[Monday morning haskell](https://mmhaskell.com/monads/tutorial)

A Monad wraps a value or a computation with a particular context.
A monad must define both

- a means of wrapping normal values in the context (`return`)
- a way of combining computations within the context (`>>=`)
