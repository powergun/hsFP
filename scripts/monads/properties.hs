#!/usr/bin/env stack runghc

{-
real world haskell P/368

monad properties:

- a type constructor m

- a function of type: m a -> (a -> m b) -> m b for chaining 
the output of one function into the input of another

- a function of type: a -> m a for injecting a normal value into
the chain, that is, it wraps a type a with the type constructor m

(refer to binparse / PNM, Parse modules)
the properties that make the Maybe type a monad are its type 
constructor Maybe a, our chaining function (>>?), and the injector
function Just

for Parse, the corresponding properties are the type constructor 
Parse a, the chaining function (==>) and the injector function 
identity

Many common programming patterns have a monadic structure:
passing around implicit data or short-circuiting a chain of 
  evaluations if one fails 

P/370
the monad instance for binparse / Parser
instance Monad Parse where
  return = identity
  (>>=) = (==>)
  fail = bail
* use a sensitive fail function, rather than the default impl
* which causes error() 

Monadic: pertaining to monads
a monadic type is an instance of the monad typeclass
a monadic value has a monadic type

when we say that a type is a monad, this is really a shorthand 
way of saying that it is an instance of the monad typeclass

-}

{-
Learn you a haskell
http://learnyouahaskell.com/a-fistful-of-monads

if you have a value with a context, m a, how do you apply to it 
  a function that takes a normal a and returns a value with a 
  context? 
This is , how do you apply a function of type a -> m b to a 
value of type m b ?

if we have a fancy value and a function that takes a normal value 
  but returns a fancy value, how do we feed that fancy value
  into the function

-}