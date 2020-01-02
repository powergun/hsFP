# Monoid & Semigroup

Monoid minus identity value

source: <https://medium.com/blacklane-engineering/pure-functional-validation-64a7885d22ac>

see also, Haskell Programming from the first Principles

## <>, mappend operator

see src/Mappend.hs

## Bool monoid & Maybe monoid

source: first principles P/614; see src/MaybeMonoid.hs

bool and maybe have more than one monoid instances; maybe has First
and Last, while bool has All and Any

the example shows that mconcat on `First` monoidal values returns
the first non-nothing value

## Monoidal composition

source: first principles P/615; recall "don't fear monad by Brian Beckman"
on youtube: monoid provides a way to combine functionalities

## NonEmpty - a semigroup that can not have monoidal instance

see src/NonEmpty.hs; note that use of `:|` operator in the data
constructor; it is equivalent to `data NonEmpty a = NonEmpty a [a]`
