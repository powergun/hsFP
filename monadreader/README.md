# monadreader

source:

Monday morning haskell

## WrapInteger

the minimal example of Reader

## WrapEnv

add more complexity to the minimal example, this shows what the
"environment" can offer,

in this case the environment is a data structure where fields
can be accessed via `Mr.asks`

there are some subtles about "record type", see:

https://gitlab.haskell.org/ghc/ghc/wikis/records

### Monday morning haskell

In order to call a reader action from pure code, all we need to do
is call the runReader function and supply the environment as a parameter.
All functions within the action will be able to treat it like a
global variable.

our other two functions no longer take the environment as an explicit
parameter. They simply exist in a context where the environment is
a global variable.

## WrapHandler

## MoveCursor

moving a virtual cursor in 2d using a list of coordinates;

this example shows that, `Control.Monad.Reader` is to provide
an access mechanism to some kind of "environment";

recall the ReaderT type signature:

`ReaderT r m a`

`r` is the environment

`m` is a monad where actions are performd...... (TODO: is that correct?)

`a` is the return type; in some of my examples, it is simply set
to `()`.

this example also shows that,
caller can design a modifiable (although not necessarily mutable)
environment e.g. using `Control.Monad.Writer`

compare this with `monadstate/MoveCursor`
