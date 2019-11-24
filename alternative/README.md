# alternative

## Alternative

source:

https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

Like most general-purpose classes, Alternative and MonadPlus are
expected to follow a handful of laws.

empty and (<|>) form a monoid

```haskell
-- empty is a neutral element
empty <|> u  =  u
u <|> empty  =  u
-- (<|>) is associative
u <|> (v <|> w)  =  (u <|> v) <|> w
```

"neutral element" and "associative" here is just like how addition
of integer numbers is said to be associative and to have zero as neutral
element. In fact, this analogy is the source of the names of the MonadPlus methods, mzero and mplus.

## MonadPlus

source:

https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus

One might legitimately wonder why the seemingly redundant MonadPlus
class exists. Part of the reason is historical: just like Monad existed
in Haskell long before Applicative was introduced, MonadPlus is much
older than Alternative. Beyond such accidents, there are additional
expectations (ones that do not apply to Alternative) about how
the MonadPlus methods should interact with the Monad, and therefore
indicating that something is a MonadPlus is a stronger claim than
indicating that it is both an Alternative and a Monad.

As for MonadPlus, at a minimum there usually are the monoid laws

```haskell
mzero `mplus` m = m
m `mplus` mzero = m
m `mplus` (n `mplus` o) = (m `mplus` n) `mplus` o

-- monadic low
mzero >>= f = mzero -- left zero
m >>= mzero = mzero -- right zero
```

**if mzero is interpreted as a failed computation, these laws
state that a failed computation in a chain leads to the failure
of the whole chain**

## useful functions

### asum

In a sense, `asum` generalizes the list-specific `concat` operation.
Indeed, the two are equivalent when lists are the `Alternative` being
used. For `Maybe`, `asum` finds the first `Just x` in the list and returns
`Nothing` if there aren't any.

It should also be mentioned that msum, available from both `Data.Foldable` and `Control.Monad`, is just asum specialised to MonadPlus.

```haskell
msum :: (MonadPlus m, Foldable t) => t (m a) -> m a
```
