# Maybe transformer

source:

realworld haskell (web version) chapter 18

https://en.wikibooks.org/wiki/Haskell/Monad_transformers

## mechanism

this monad transformer modifies the behaviour of an underlying
monad `m a` by wrapping its type parameter with Maybe, to give
`m (Maybe a)`

as with the Maybe monad, if we call fail in the `MaybeT` monad
transformer, execution terminates early

## example: password validation

see PasswordValidation.hs
