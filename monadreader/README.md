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

## ReaderT Pattern

source

https://www.fpcomplete.com/blog/2017/06/readert-design-pattern

Often times I'll receive or read questions online about "design
patterns" in Haskell. A common response is that Haskell doesn't
have them. What many languages address via patterns, in Haskell
we address via language features (like built-in immutability,
lambdas, laziness, etc). However, I believe there is still
room for some high-level guidance on structuring programs,
which I'll loosely refer to as a Haskell design pattern.

- a core data type, that contains all runtime conf and global
  functions that are **mockable**
- use mutable reference in the core data for mutability
- app code live in `ReaderT Env IO`
- use additional monad transformer for small subsets of app
- (optional) use mtl-style typeclasses, like MonadReader and
  MonadIO; can recover some purity (compensating IO and Ref)

**in haskell, we'd rather face compile time rather than runtime pain!**

ReaderT has a huge advantage over the other three transformers
listed: it has no mutable state. It's simply a convenient manner
of passing an extra parameter to all functions. And even if that
parameter contains mutable references, that parameter itself is
fully immutable.
