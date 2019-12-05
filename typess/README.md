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

type constructors can take types as parameters
to produce new types.

recall templates in C++

```haskell
data Probably a = Noway | Bingo a

λ> a = Bingo 'a'
λ> :t a
a :: Probably Char
```

the type inference engine figures the concrete type
must be `Maybe [Char]` because if the `a` in
`Bingo a` is a `Char`, then the `a` in `Probably a`
must also be a string

**list as a parameterized type**: it types a parameter to produce a concrete type,

values can have an `[Int]` type, a `[Char]` type, a
`[[String]]` type, but you can't have a value that
just has a type of `[]`

notice that the type of `Nothing` is `Probably a`.
Its type is polymorphic.
if some function requires a `Probably Int` as a
parameter, we can give it a `Nothing` because a `Nothing`
doesn't contain a value anyway and so it doesn't matter

## type parameters in typeclass instance declaration

Let’s say I’m writing a `Maybe` monad from scratch:

```haskell
newtype Maybe a = Nothing | Just a
```

As we know at this stage `Maybe` is an parameterized type;

Then I’m going to define the functor instance for my `Maybe` by
copying the textbook example:

```haskell
instance Functor Maybe where
--                   ^^^ where is the 'a' going????
    fmap = ...
```

But shouldn’t we write instead, `instance Functor (Maybe a) where`,
considering `Maybe a` is the concrete type we can use in the
declarations?

### understand the typeclass definition of Functor

https://wiki.haskell.org/Functor

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
    (<$) :: a -> f b -> f a
```

the declaration does not ask for the "content" of `f`

compare this with `Eq` typeclass:

http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Eq.html

```haskell
class Eq a where
    ...
```

Answer from Shine@Slack

> Maybe is the Functor not Maybe a. Functor is the container without
> the contained type specified.
> And the f in class Functor f where .... has a kind of `* -> *`
> if you are wondering why can Ghc know the f in “class Functor f where”
> has a kind of `* -> *` This is because the fmap type signature says
> “(a->b) -> f a -> f b”. f here is used as 1 arity type constructor
> hence `* -> *`

Recall that all the monad transformer examples I have seen, follow
this concept "monad/functor is the container"; (see `src/monadtransformer`)
therefore the monad transformer's functor instance is written:

```haskell
instance Monad m => Functor (StateT s m) where
    ...
```

StateT is a container that wraps a monad m - that's it. As to the
content of the monad m, we don't need to know here due to the nature
of monad transformer - it enables encapsulation.

## Typeclass constraint

source: learn you a haskell

### when using typeclass constraint is not a common practice

see `src/TypeclassInstance.hs`

### when I must use typeclass constraint

most of the times, class constraints in class decl
are used for making a typeclass a subclass of another
typeclass;

class constraints in instance decl are used to express
requirements about the contents of some type,
e.g. `src/TypeclassInstance.hs`, we require the
contents of the `Probably` to also be part of `Eq`
typeclass

### inspect all typeclass functions

in ghci, run `:info YourTypeClass`

### typeclass constraint from my own understanding

see: src/Typeclass/Constraint.hs

this is a utterly useless example documenting my understanding

- constraint must be a typeclass (not a type!)
- the constraint could be a monad
- calling the typeclass method using a type that does not fulfil
  the constraint will not typecheck
- type parameter must be respected; make a clear distinction between
  value-constructor layout (implementation leaking) and type parameter
