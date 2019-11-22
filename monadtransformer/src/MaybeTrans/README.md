# Maybe transformer

source:

[realworld haskell (web version) chapter 18](https://en.wikibooks.org/wiki/Haskell/Monad_transformers)

[Monday morning haskell](https://mmhaskell.com/monads/transformers)

## mechanism

this monad transformer modifies the behaviour of an underlying
monad `m a` by wrapping its type parameter with Maybe, to give
`m (Maybe a)`

as with the Maybe monad, if we call fail in the `MaybeT` monad
transformer, execution terminates early

## example: password validation

with monads, a natural things we might want to do is using the capabilities 
of several monads at once,

for instance, a function could use both IO and Maybe exception handling

while a type like `IO (Maybe a)` would work just fine, it would **force
us to do pattern matching within IO do-blocks** to extract values, something
that Maybe monad was meant to spare us from.

see PasswordValidation.hs

see also **Monady morning haskell** for how to use a single pattern-matching
with runMaybeT - see `patternMatch` demo in `PasswordValidation.hs`

pay attention to the robust use of `<-` and `runMaybeT`
