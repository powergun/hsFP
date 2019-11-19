# notes

## video | monad transformer state

### 00:57

monad transformer is something one uses to add extra functionality on top 
of existing monad; this provides convenience (there could be other way to
achieve that); avoid passing extra arguments to function

Canonical example (of monad transformer) is ReaderT 

The concept of environment and state, how do they apply to monad transformer

ReaderT, StateT, ExceptT

(IdentityT, WriterT, LoggingT, MaybeT: are isomorphic to something above)

Multiple layers of monad transformers are called "transformer stack"

### 02:39

all these three transformers are newtype wrappers around something going on
with the underlying monad

```haskell
newtype ReaderT r m a = ReaderT (r -> m a)
newtype StateT s m a = StateT (s -> m (a, s))
newtype ExceptT e m a = Except (    m (Either e a ))
```

ReaderT has a transformer environment (the r), 
but does not modify the environment nor the output (m a)
this is the beauty of ReaderT - it is read only (no mutation)

StateT also has s (similar to ReaderT's r) in its input, and also has it 
in the output; 
since we can change s in the body of the function, we have a state

ExceptT has no environment, but has an output state (m (Either e a)) instead 
of m a

StateT and ExceptT both have mutable state

Because ReaderT does not have (mutable) state, concurrently works with it
and we can implement control functions with it

can specialize them to IO and turn them into functions

```haskell
type ReaderIO r a = r -> IO a
type StateIO s a = s -> IO (a, s)
type ExceptIO e a =     IO (Either e a)
```

### 04:58

a real world motivation

hate to do this - explicitly passing MyEnv to foo and bar

```haskell
foo :: MyEnv -> IO a
bar :: MyEnv -> IO b

baz :: MyEnv -> IO (a, b)
baz myEnv = concurrently (foo myEnv) (bar myEnv)
```

getting rid of that extra argument myEnv, using ReaderT; don't have function
anymore but only ReaderT action (see foo, bar below)

```haskell
foo :: ReaderT MyEnv IO a
bar :: ReaderT MyEnv IO b

-- THIS WON'T WORK
-- baz except IO but not a ReaderT on top of IO
baz :: ReaderT MyEnv IO (a, b)
baz = concurrently foo bar 
```

ReaderT is a convenient way to avoid argument passing BUT THIS IS NOT NICE

```haskell
concurrentlyR :: ReaderT env IO a -> ReaderT env IO b -> ReaderT env IO (a, b)
concurrentlyR (ReaderT foo) (ReaderT bar) = ReaderT $ \env -> concurrently (foo env) (bar env)
```

don't need to use explicit data constructor unwrapping

runReaderT unwraps foo and bar; ask to grab the environment

```haskell
concurrently R foo bar = do
  env <- ask
  lift $ concurrently (runReaderT foo env) (runReaderT bar env)
```

problem: do we want to redefine every single function in the entire 
code base everytime we want to use monad transformer

### 19:44

ReaderT-ish things:

Any transformer without state (but only some kind of input values) is 
isomorphic to ReaderT, examples:

IdentityT (pretend () is the environment)

LoggingT (the logging function is the environment)

NoLoggingT (it is just a newtype on IdentityT); both LoggingT and NoLoggingT
come from monad logger library

If you can take your monad transformer and represent it somehow in terms 
of the ReaderT, you are safe and can do unlift with it;



