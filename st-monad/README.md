# ST Monad

## First Principles P/1140

This chapter is not dedicated to ST Monad, but about containers;
ST Monad is introduced here to demonstrate the performance benefit
of using local mutable state.

P/1140

> ST can be thought of as a mutable variant of the strict State monad.
> From another angle, it could be thought of as IO restricted exclusively
> to mutation which is guaranteed safe.

morally effect-free P/1141

> it unfreezes your data, mutates it then freezes it again so it can't
> be mutated anymore;
> Thus it manages to mutate and still maintain referential transparency

compiler's responsibility

> For ST to work properly, the code that mutates the data must not get
> reordered by the optimizer or otherwise monkeyed with, much like the code
> in IO. So there must be something underlying the type which prevents GHC
> ruining our day.

what is ST's side effect

> It's important to understand that s isn't the state you're mutating.
> The mutation is a side effect of having entered the closures that perform the effect.

what is the `s` type for

> ST enforces it at compile-time by making it so that s will never unify
> with anything outside of the ST Monad. The trick for this is called
> existential quantification
> it does prevent you accidentally leaking mutable references to code outside ST

P/1142

> Avoid dipping in and out of ST over and over. The thaws and freezes will cost
> you in situations where it might have been better to just use IO. Batching your
> mutation is best for performance and code comprehensibility.

benchmark from `hsAlgorithms/containerss/src/FirstPrinciples/VectorMutable.hs`
shows IO is faster than ST

```text
mutableUpdateST 32
mutableUpdateIO 19
```
