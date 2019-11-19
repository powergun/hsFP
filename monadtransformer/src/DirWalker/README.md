# Directory Walker

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
