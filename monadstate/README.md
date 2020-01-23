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

> (P/898) Newtypes must have the same underlying representation as the type
> they wrap, as the newtype wrapper disappears at compile time. So
> the function contained in the newtype must be isomorphic to the
> type it wraps. That is, there must be a way to go from the newtype
> to the thing it wraps and back again without losing information.

### State is a function

> (P/899) State is a function that takes input state and returns an output
> value, a, tupled with the new state value.
> The key is that the previous state value from each application is chained to
> the next one, and this is not an uncommon pattern.

see `src/FirstPrinciples/RollDiceState.hs` for an example - this also touches
monad transformer

> (P902) For our purposes, the state function is a constructor that takes
> a State-like function and embeds it in the State monad transformer.

the State-like function is `randomR (1, 6)`, which expects a State-like
value and produces the tuple...

### Repeat vs ReplicateM

P/903 to implement a "roll n times" solution...
`repeat <$> <state>` will ONLY repeat a **static value**;
whereas replicateM does the right thing:
`replicateM n act performs the action n times, gathering the results.`

### State Functor, Applicative and Monad

P/905

recall **a function is also a structure**;

lift f and apply it to the existing value inside the structure;
the structure is a function-application of (g s);
the value is the first element of the result of (g s);

see: `src/FirstPrinciples/StateFromScratch.hs`

this is also another good exercise to implement getter, setter,
eval and exec functions.

### Using State from transformers package

P/908

see: `src/FirstPrinciples/Fizzfuzz.hs`
