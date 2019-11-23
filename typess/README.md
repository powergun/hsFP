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

to export the value constructor, write `..`;

we could also opt not to export any value constructor by just
writing the type name in the export statement;
that way someone importing our module could only making instances
of our types by using the auxiliary functions (factory functions)

`Data.Map` uses that approach - you can't create a map by doing
`Map.Map ...` because it does not export that value constructor.

You can make a mapping by using one of the auxiliary functions like
`Map.fromList`

Not exporting the value constructor of a data types makes them more
abstract in such a way that we hide their implementation.
Also whoever uses our module can't pattern match against the value
constructors

## Type parameters
