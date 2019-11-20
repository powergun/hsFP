# Transform stack: Stacking multiple monad transformers

source: real world haskell (web version) chapter 18

under what circumstance do we need such stack?

- if we need to talk to the outside world, we'll have IO at the base
of the stack; otherwise we will have some normal monad
- if we add ReaderT layer, we give ourselves access to read-only
configuration information
- add a StateT layer, and we gain global state that we can modify
- should we need the ability to log events, we can add WriterT layer

**MY NOTE**: THIS IS THE PATTERN LANGAUGE I HAVE BEEN LOOKING FOR
