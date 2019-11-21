# Monad Writer, Monad Writer Transformer

source:

[Monday Morning Haskell](https://mmhaskell.com/monads/reader-writer)

The Writer monad is parameterized by some monoidal type.

Its main job is to keep track of an accumulated value of this type.

So it’s operations live in the context of having a global value that
they can modify in this particular way.

## Transactions

using the above concept, this example shows how to keep track of
bank account transactions and provide a way to summarise the account
balance
