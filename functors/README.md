# functors

## Bifunctors

inspired by various sources, particularly hsSysAdmin/thecli (which uses
bifunctor to transform the Exception value inside an Either value)

## Contravariants

inspired by a blogspot; this is still "on the level of useless",
but it can be useful to model pre-conditions

## Derive Functors

this is to show that a newtype that wraps a primative type can become
a functor for free (since this newtype acts like a "container")

## WTF is `$>`

see a use case in: <https://medium.com/blacklane-engineering/pure-functional-validation-64a7885d22ac>

it helps to write more concise validation logic

it is the inverse of `<$`, which:
Replace all locations in the input with the same value. The default definition is fmap . const, but this may be overridden with a more efficient version.

`$> :: Functor f => f a -> b -> f b`

```haskell
[12] $> 1
[1]
```
