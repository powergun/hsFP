# State Monad, State Monad Transformer

source:

[Monday Morning Haskell](https://mmhaskell.com/monads/state)

The State monad wraps computations in the context of reading and
modifying a global state object.

we observe there will still be a final return type on each expression
in State, just as there is in any other monad. Thus our different
function types will look like this for a return type of a:

`State GameState a`
