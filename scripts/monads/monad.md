# what is monad

[notes taken from](https://www.haskell.org/tutorial/monads.html)

## bind operator

The bind operations, >> and >>=, combine two monadic values while
the return operation injects a value into the monad (container).

The signature of >>= helps us to understand this operation:

```haskell
ma >>= \v -> mb
```

combines a monadic value ma containing values of type a and a function which operates on a value v of type a, returning the
monadic value mb.

The result is to combine ma and mb into a monadic value containing
b.

The >> function is used when the function does not need the value produced by the first monadic operator.

__NOTES__ recall the notion of "passing state in and out" from various
stackoverflow posts

The precise meaning of binding depends, of course, on the monad.

For example, in the IO monad, x >>= y performs two actions sequentially, passing the result of the first into the second.

For the other built-in monads, lists and the Maybe type, these monadic operations can be understood in terms of passing zero
or more values from one calculation to the next.

The do syntax provides a simple shorthand for chains of monadic
operations. The essential translation of do is captured in the
following two rules:

```haskell
  do e1 ; e2      =        e1 >> e2
  do p <- e1; e2  =        e1 >>= \p -> e2

  e1 >>= (\v -> case v of p -> e2;
                          _ -> fail "s")
```
