# applicative

source:

https://mmhaskell.com/monads/applicatives

## ParserMonad

a monadic parsing example; it implements a mini-parsec from scratch

## When functor can not do the job (Monady morning haskell)

However, what happens when we try to combine wrapped data?
For instance, if we try to have GHCI interpret these calculations,
we’ll get type errors:

```haskell
>> (Just 4) * (Just 5)
>> Nothing * (Just 2)
```

## Parallel application functions (Monday morning haskell)

**MY NOTE**: this is touched in haskell class

```haskell
-- see homework 03 reference
-- $ zip3 l (drop 1 l) (drop 2 $ l) l
-- . (zip3 <$> id <*> drop 1 <*> drop 2)
. (zip3 <*> drop 1 <*> drop 2)

-- id can be dropped from the definition because:

-- λ> f = zip3 <$> id
-- λ> :t f
-- f :: [a] -> [b] -> [c] -> [(a, b, c)]
-- λ> :t zip3
-- zip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
```

You might be wondering how we might do parallel application of functions.
For instance, we might want to use the second list example above, but
have the result be [2,10,30]. There is a construct for this, called
`ZipList` ! It is a newtype around list, whose Applicative instance
is **designed to use this behavior.**

```haskell
>> ZipList [(1+), (5*), (10*)] <*> [5,10,15]
ZipList {getZipList = [6,50,150]}
```

