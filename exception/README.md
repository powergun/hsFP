# exception handling

## Control.Exception.try - an exception handler using Either type

package is `Control.Exception`, part of base;

note, `Data.Bifunctor` (as shown in hsSysAdmin/thecli project),
can be a helpful campanion to the Either type:

`first()` (and also `second()`) function can work on the first
(or second) argument contained in the given Either value, this
means I can developed a failure or success handler that only
process the failure or success, leaving the another part unchanged

```haskell
first ::  Bifunctor p => (a -> b) -> p a c -> p b c
second :: Bifunctor p => (b -> c) -> p a b -> p a c

-- example
λ> :t a
a :: Either Int String

-- only change the failure, turning it into a String
λ> a
Left (-99)
λ> first show a
Left "-99"

-- only change the success, turning it into a String
λ> a = Right "iddqd"
λ> first show a
Right "iddqd"
```
