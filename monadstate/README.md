# State Monad, State Monad Transformer

source:

[Monday Morning Haskell](https://mmhaskell.com/monads/state)

The State monad wraps computations in the context of reading and
modifying a global state object.

we observe there will still be a final return type on each expression
in State, just as there is in any other monad. Thus our different
function types will look like this for a return type of a:

`State GameState a`

## Rediscover State (monad), First Principles P/894

source: First Principles P/894

> We can think of state as data that exists in addition to the inputs
> and outputs of our functions, data that can potentially change after
> each function is evaluated

see: src/FirstPrinciples/Random.hs to understand the design motivation
behind the `State` newtype;
note the similarity between `StdGen -> (a, StdGen)` and `a -> (a, s)`

> Newtypes must have the same underlying representation as the type
> they wrap, as the newtype wrapper disappears at compile time. So
> the function contained in the newtype must be isomorphic to the
> type it wraps. That is, there must be a way to go from the newtype
> to the thing it wraps and back again without losing information.
