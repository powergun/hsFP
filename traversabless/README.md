# Traversables

## Discover Traversable - First Principles

source: First Principles P/847

### Similarity to `=<<` and fmap

> We’re still mapping a function over some embedded value(s), like fmap, but similar to flip bind, that function is itself generating more structure.

### A Generalized mapM

> We can think of traverse in Traversable as abstracting the [] in mapM to being any Traversable data structure and generalizing the Monad requirement to only need an Applicative.

```haskell
λ> mapM print [1..3]
1
2
3
[(),(),()]
λ> traverse print [1..3]
1
2
3
[(),(),()]
```

> For example, the list datatype is fine for small pluralities of values but in more performance-sensitive code, you may want to use a Vector from the `vector` library. With traverse, you won't have to change your code because the primary Vector datatype has a Traversable instance and so should work fine.

### what is Traversable for

> In a literal sense, anytime you need to flip two type constructors around, or map something and then flip them around, that’s probably Traversable:

```haskell
λ> sequenceA $ Just [1]
[Just 1]

λ> let f = undefined :: a -> Maybe b
λ> let xs = undefined :: [a]
λ> :t map f xs
map f xs :: [Maybe b]
λ> :t traverse f xs
traverse f xs :: Maybe [b]
```

**MY NOTES**: on `sequenceA`:

Given `a collection` of structure `f` that holds a unit value `a`, scale it to a structure (of the same type) that holds `a collection of a values`

However any `Left value` in the sequence/collection will cause the final
result to also be a `Left value`, which is the applicative nature

**MY NOTES**: on `traverse`:

Given a unit operation, `a -> f b` that produces a structure `f b`,
and given a `collection of a values`, create a structure of same type
 `f` that contains `collection of b values`; (Note that First Principles
uses `sequence` instead of `collection`)

essentially scaling the content of structure `f` from a unit value to
a `collection` (which can be a structure too)

> We don't want catMaybes here because it drops the Nothing values.
What we want here is for any Nothing values to make the final result
Nothing. The function that gives us what we want for this is sequence.

see: src/DBQueryPipeline.hs; source: First Principle P/854

note how it "scale" a simple value, via a sequence/collection to a
structure of sequence/collection of values

> but what if we don't want a list of IO actions we can perform to
get a response, but rather one big IO action that produces a list
of response? This is where Traversable can be helpful

see: src/HttpQuery.hs; source: First Principles P/855

### Traversable is Stronger than Functor and Foldable

P/856

> because of this we can recover the functor and foldable instance
> for a type from the Traversable

see: src/RedoFunctor.hs; Note how fmap is re-implemented in terms of
`traverse`; Note the use of Identity and `runIdentity` to make a
dummy structure.

```haskell
λ> import Data.Monoid
λ> [1, 2, 3] :: [Sum Int]
[Sum {getSum = 1},Sum {getSum = 2},Sum {getSum = 3}]
λ> import Data.Functor.Constant
λ> xs = [1, 2, 3] :: [Sum Int]
λ> f = (+1)
λ> :t traverse (Constant . f) xs
traverse (Constant . f) xs :: Constant (Sum Int) [b]
```

Recall that Traversable works with Applicative **which has monoidal
behavior on the structure part**; `Constant (Sum Int) [b]` this is the
result of the monoidal part so that the list of `Sum Int` via the
monoidal behavior of the structure, becomes a single `Sum Int`;
An extremely simple example of this is `traverse (\n -> [n]) [1..2]`
which produces `[[1,2]]` instead of `[[1], [2]]` because list is an
instance of Monoid (inner) and Applicative (outer);

### Traversable Laws

#### traverse()

Naturalaity: `t . traverse f = traverse (t . f)`

Identity: `traverse Identity = Identity`

> Identity represents a structural identity for traversing data
> this is another way of saying that a Traversable instance can not add or inject any structure or "effects"

```haskell
λ> import Data.Functor.Identity
λ> traverse Identity [1..10]
Identity [1,2,3,4,5,6,7,8,9,10]
```

to quick recap the Monoidal behavior on the (list) structure:

```haskell
λ> [1] `mappend` [2]
[1,2]
λ>
```

Composition: using `Compose` datatype, P/860

#### sequenceA()

Naturality: `t . sequenceA = sequenceA . fmap t`

Identity: `sequenceA . fmap Identity = Identity`

Composition

#### Use QuickCheck to test laws

see: test/DemoQuickCheckTraversableLaws.hs

also implement the chapter exercise on P/862
to provide Functor, Foldable and Traversable instance
for a Binary Tree

I found a reference here: <https://github.com/cwyang/haskell/blob/master/functor.hs>

here is the official reference for foldable tree: <https://hackage.haskell.org/package/base-4.12.0.0/docs/Prelude.html#g:11>
