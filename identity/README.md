# Identity Monad

why is identity useful??

see: https://stackoverflow.com/questions/28645505/why-is-identity-monad-useful

Identity is to monads, functors and applicative functors as 0 is to numbers. On its own it seems useless, but it's often needed in places where one expects a monad or an (applicative) functor that actually doesn't do anything.

As already mentioned, Identity allows us to define just monad transformers and then define their corresponding monads just as SomeT Identity.

But that's not all. It's often convenient to also define other concepts in terms of monads, which usually adds a lot of flexibility. For example Conduit i m o (also see this tutorial) defines an element in a pipeline that can request data of type i, can produce data of type o, and uses monad m for internal processing. Then such a pipeline can be run in the given monad using

($$) :: Monad m => Source m a -> Sink a m b -> m b
(where Source is an alias for Conduit with no input and Sink for Conduit with no output). And when no effectful computations are needed in the pipeline, just pure code, we just specialize m to Identity and run such a pipeline as

runIdentity (source $$ sink)

see also: https://tech.fpcomplete.com/haskell/tutorial/monad-transformers

