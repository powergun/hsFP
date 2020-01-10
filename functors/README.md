# functors

**how to display all Functor instances in ghci?**

see: <https://stackoverflow.com/questions/25395091/have-ghci-list-all-possible-type-class-instances>
`:i Functor`; same applies for Applicative and Monad

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

## Rediscover functor (first priniciples)

source: first priniciples P/649

read this again and again, and again: first principles P/697, 16.18 definitions

> we recall that a type constant or a fully applied type has the `kind *`.
> A type with kind `* -> *` is awaiting application to a type constant of `kind *`
> The `type f` was applied to a single argument in two different places: `f a`
> and `f b`. Since `f a` and `f b` must each have the `kind *`, f by itself must be `kind * -> *`

### why there is no `a` in the Functor instance

MY NOTE: this confused me before (while reading Monad)

source: first principles P/656; see: src/Structures.hs

```haskell
instance Functor (FixMePls a) where
    fmap _ FixMe = FixMe
    fmap f (Pls a) = Pls (f a)
```

Notice we didn’t change the type; it still only takes one argument. But now that argument is part of the f structure. If we load this ill-conceived code:

We get the same error as earlier, because applying the type constructor gave us something of `kind *` from the original `kind of * -> *`.

type constants (`*`) v.s. type constructors (`* -> *`)

> Now we have a means of talking about the contents of types independently from the type that structures those contents. That’s why we can have something like fmap that allows us to alter the contents of a value without altering the structure (a list, or a Just) around the value.

### when there are s e a ... in the Functor instance

source: first principles P/662; see: src/Structures.hs

MY NOTE: Monad reader and other more complex types require more than
one type argument - all but the last one **belong to the structure**!!!

(and Functor does not change the structure)

> Just stop messing with the Int in Heisenberg. Think of anything that isn’t the final type argument of our f in Functor as being part of the structure that the functions being lifted should be oblivious to.

this fact is more prominent is the case of Tuple and Either values

also: partial application (common in functions) works on the type level too,
`* -> * -> *` can be reduced to `* -> *`

P/675, further clarify the difference between type-level argument and
term-level argument:

```haskell
instance Functor (Two a) where
    fmap f (Two a b) = Two $ (f a) (f b)
```

> This won’t fly either, because the a is part of the functorial structure, the f. We’re not supposed to touch anything in the f referenced in the type of fmap, so we can’t apply the function (named fin our fmap definition) to the a because the a is now untouchable.

MY NOTE: a in `Two a` is a type level arg; a in `fmap f (Two a b)` is to
decompose the data ctor which uses the term level arg

### function is also structure

see: src/Structures.hs; briefly mentioned in the same section in
first principles

> the goal of fmapping is to leave the outer structure untouched while transforming the type arguments inside.

### what is (fmap . fmap)

```text
λ> f = (fmap . fmap)
λ> :t f
f :: (Functor f1, Functor f2) => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
```

note how f2 is seen nested in f1

### natural transformation (opposite of functor)

source: first principles P/690

why `nat :: (f -> g) -> f a -> g a` won't work: f, g are higher-kinded
types

> Syntactically, it lets us avoid talking about a in the type of Nat, which is what we want, we shouldn’t have any specific information about the contents of f and g because we’re supposed to be only performing a structural transformation, not a fold.

## Function Composition via fmap(): First Principles P/866

beside normal composition via operator: `(+1) . (*2)`, composing
two functions can be written as `fmap (+1) (*2)`

```haskell
λ> :t fmap (+1) (*2)
fmap (+1) (*2) :: Num b => b -> b
λ> :t (+1).(*2)
(+1).(*2) :: Num c => c -> c
```

(explaining a (partially-applied) function is a structure, aka functorial
context)

> the (functorial) context (aka the structure) is a partially-applied
> function. As in function composition, fmap() composes the two
> functions before applying them to the argument.
> this is the functor of functions
