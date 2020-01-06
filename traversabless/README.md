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
