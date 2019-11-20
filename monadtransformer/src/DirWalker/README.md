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

## TransStackImpl

This uses the concept of "Monad Transformer Stack".

See also the dedicated [sub-project: TransformerStack](../TransformerStack)

We use ReaderT to store config data; use StateT to record
the maximum depth during the actual traversal

In this example it does not matter whether we have ReaderT or
WriterT on top, but IO must be at the bottom.

**MY NOTE**: the order of the stack still matters

### where is the missing type parameter 'a'

why not write

`data App a = Mr.ReaderT AppConfig (Ms.StateT AppState IO) a`

the difference arises when we try to construct another type from
one of these, say we want to add another layer to the stack, the
compiler will allow `WriterT [String] App a` (if App does not
carry a)

Haskell does not allow us to partially apply a type synonym,
The synonym App does not take a type parameter, so it does not
pose a problem; however when it does take a parameter `a`, we
must supply some type for that parameter if we want to use App
to create another type;

this restriction is limited to type synonyms; when we create a
monad transformer stack, we usually wrap it with a newtype;

as a result we rarely run into this problem in practice...

