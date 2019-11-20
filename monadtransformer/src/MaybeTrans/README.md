# Maybe transformer

source: realworld haskell (web version) chapter 18

this monad transformer modifies the behaviour of an underlying
monad `m a` by wrapping its type parameter with Maybe, to give
`m (Maybe a)`

as with the Maybe monad, if we call fail in the `MaybeT` monad
transformer, execution terminates early


