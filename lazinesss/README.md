# laziness

## First Principles, P/1064

### Force non-lazy

P/1065

```haskell
hs> import Data.Bool (bool)
hs> y = 1
hs> x = undefined
hs> bool x 1 (y == 1)
1
hs> bool x 1 (x `seq` y == 1)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
  undefined, called at <interactive>:18:5 in interactive:Ghci11
hs>
```

`seq` force Haskell to evaluate `x` at the same time it also evaluates `y`,

> Because evaluation in Haskell is demand driven, we can't guarantee that
> something will ever be evaluated period. Instead we have to create links
> between nodes in the graph of expressions where forcing one expression will
> force yet another expression.

```haskell
undefined `seq` y `seq` x
```

> forcing x necessarily forces y
> forcing y necessarily forces undefined
> if you want to get him, you gotta get through me!

High Perf Haskell P/38

`seq` evaluates its argument to WHNF only; observe the second `sprint`
that only prints UP TO the first data ctor

```haskell
hs> t = const (Just 1) () :: Maybe Int
hs> :sprint t
t = _
hs> t `seq` ()
()
hs> :sprint t
t = Just _
```

use `$!` (see hoogle), strict call by value application operator

### Weak head normal form (WHNF)

> WHNF evaluation means it stops at the first data constructor or lambda.

```haskell
dc = (,) undefined undefined
hs> dc `seq` 1
1

lamb = \_ -> undefined
hs> lamb `seq` 1
1
```

`dc` is a data ctor - weak head normal form, seq does not care about the
value inside

### "Core Dump"

P/1070

```haskell
:set -ddump-simpl
:l FirstPrinciples.CoreDump
:set -dsuppress-all
:reload FirstPrinciples.CoreDump
```

> Core and Haskell are not the same language, but anytime you need to
> know if two expressions in Haskell are the same, one way to know for
> sure is by examining the Core.

### Thunk

P/1078

GHCi's sprint:

```haskell
hs> a = (,) undefined undefined
hs> :sprint a
a = _
```

> GHC will also stop opportunistically evaluating as soon as it hits a
> computation

```haskell
hs> let a = [1, 2, id 1] :: [Integer]
hs> :sprint a
a = [1,2,_]
```

### Debug.Trace

P/1081

```haskell
hs> import Debug.Trace
hs> let a = trace "a" 1
hs> let b = trace "b" 2
hs> a + b
b
a
3
```

### Sharing

> Sharing doesn't work in the presence of constraints (typeclasses or
> implicit parameters) because typeclass constraints and implicit parameters
> decay into function arguments when the compiler simplifies the code

### Bang Pattern

P/1096

> The idea here is that in some cases, it's cheaper to just compute
> something than to construct a thunk and then evaluate it later.
> A good rule to follow is lazy in the spine, strict in the leaves!

recall that this is why most record types in rio's examples use Bang-Pattern;

### Strict Pragma

```haskell
hs> bl x = 1
hs> print (bl undefined)
*** Exception: Prelude.undefined
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/Err.hs:78:14 in base:GHC.Err
  undefined, called at <interactive>:4:11 in interactive:Ghci3
```

> the Strict pragma and will get translated into the following

```haskell
blah x = x `seq` 1
```

## Lazy Pattern, High Perf Haskell P/155

> We'll start with lazy patterns. Where strict pattern annotations
> use bangs and mean "Evaluate this argument to WHNF immediately,"
> lazy pattern annotations use tildes and imply "Don't even bother
> pattern-matching unless a binding is really requested."
