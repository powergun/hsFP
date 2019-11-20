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

## lift

provided by MonadTrans typeclass - every monad transformer is an
instance of MonadTrans

we use the name lift based on its similarity of purpose to fmap
and liftM;

in each case, we hoist something from a lower level of the type
system to the level we are currently working in

fmap: elevates a pure function to the level of functor

liftM: takes a pure function to the level of monad

lift: raises a monadic action from one level beneath in the stack
to the current one

### when explicitly lifting is necessary

one case in which we must use lift is when we create a monad
transformer stack in which instances of the same typeclass appear
at multiple levels

### avoid "lift . lift . lift"

sometimes we need to access a monad more than one level down the
stack, in which case we must compose calls to lift:

`lift . lift . lift`

it can be good style to write wrapper function that does the lifting for us;

avoid hard-wiring the details of the layout of our monad stack
into our code, which will complicate any subsequent modifications

