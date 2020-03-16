# Lens

## Hackage Lens Tutorial

source: <http://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html>

> what is a lens: lens is a first class getter and setter
> in other worlds, lenses package both "get" and "set" functionality
> into a single value (the lens)
> you could **pretend** that a lens is a record with two fields

```haskell
data Lens a b = Lens
    { view :: a -> b
    , over :: (b -> b) -> (a -> a)
    }
```

> the actual definition of Lens' is

```haskell
type Lens' a b = forall f . Functor f => (b -> f b) -> (a -> f a)
```

> we get to pick what Functor we specialize f to and
> depending on which Functor we pick we get different
> features

> You can think of the function composition operator as having this type:

```haskell
(.) :: Lens' a b -> Lens' b c -> Lens' a c
```

### Traversals

traversal: a first class getter and setter for an arbitrary number of values

a traversal lets you get all the values it points to as a list and it also lets
you update or set all the value it points to

```haskell
type Traversal' a b = forall f . Applicative f => (b -> f b) -> (a -> f a)
```
