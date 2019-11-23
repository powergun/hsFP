# Type, Type Parameter, Typeclass, Typeclass Parameter

source:

[Learn you a haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)

## Algebraic Data Type (ADT)

now when I say fields, I acually mean parameters.
**value constructors** are actually functions that
ultimately **return a value of a data type**

```haskell
λ> :t Circle
Circle :: Float -> Float -> Float -> Shape
λ> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

we can pattern match against constructors

```haskell
f (Circle _ _ r) = ...
f (Rectangle x1 y2 x2 y2) = ...
```
