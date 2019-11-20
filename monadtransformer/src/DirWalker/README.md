# Directory Walker

## WriterImpl

source: [real world haskell (web version) chapter 18](http://book.realworldhaskell.org/read/monad-transformers.html)

the normal writer monad has two type parameters: `Writer w a`

w is the type of the values to be recorded;

a is the usual type that the `Monad typeclass` requires

thus `Writer [(FilePath, Int)] a` is a writer monad that records
a list of directory names and sizes

`WriterT` transformer has a similar structure but it adds another
type parameter `m`: this is the underlying monad whose behavior
we are augmenting - **stacking the Writer monad on top of the IO monad;
the stack of monad transformer and monad is itself a monad**

the full signature of WriterT is `WriterT w m a`

[Why use liftIO (see lift vs liftIO section)](https://www.schoolofhaskell.com/user/commercial/content/monad-transformers)

This is where liftIO helps us. **It essentially lets us do a variable
number of lifts.** This lets us write less brittle code because if we
decided to add yet another layer to our transformer stack, we wouldn't
have to hardcode another call to lift.

there is no "IOT" transformer; whenever we use IO monad with one or
more monad transformers, IO will always be at the bottom of the stack

`runWriterT` gives both the result of the action and what was recorded

`execWriterT` throws away the result and just gives us what was recorded

### Common patterns in monads and monad transformers

there usually exist both a concrete monad and a transformer, each of which
are instances of the typeclass that defines the monad's API




