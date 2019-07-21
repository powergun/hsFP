#!/usr/bin/env stack runghc

-- haskell first principles P/602
-- in haskell these algebras can be implemented with typeclasses
-- the typeclasses define the set of operations; when we talk
-- about operations over a set, the set is the type the operations
-- are for. The instance defines how each operation will perform
-- for a given type or set

-- P/603
-- typeclasses give us a way to recognize, organize and use
-- common functionalities and patterns across types that differ
-- in some ways but also have things in common

-- the typeclass abstracts and generalizes the pattern so that
-- you write code in terms of any type that can be monoidally
-- combined

-- P/604
-- in haskell we think of types as having an instance of a
-- typeclass;
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- real world haskell P/176
-- by having a generic function that can compare anything,
-- we can also make our code generic:
-- if a piece of code needs only to compare things, then it
-- ought to be able to accept any data type that the compiler
-- knows how to compare
-- what's more, if new data types are added later, the existing
-- code shouldn't have to be modified

-- haskell's typeclasses are designed to adress these things
-- READ:
-- rust's trait vs haskell's typeclass
-- https://gist.github.com/DarinM223/f6adf64569b55408886313cd3032c7e6

-- when you list the types of your functions, you must use that -- name to refer to instance types.
-- P/177
-- for any type a, so long as a is an instance of BasicEq,
-- isEqual takes two parameters of type a and returns a Bool
class BasicEq a where
  isEqual :: a -> a -> Bool

  -- superEqual has a default implementation
  superEqual :: a -> a -> Bool
  superEqual x y = isEqual x y

instance BasicEq Bool where
  isEqual True True   = True
  isEqual False False = True
  isEqual _ _         = False

-- real world haskell P/192
-- haskell's typeclasses are intentionally designed to let us create
-- new instance of a typeclass whenever we see fit
-- NOTE: I had some issue with this instance function, but it
-- turned out that I need to use parentesse for numeric literal
-- such as (3 :: Double) at the callsites
instance BasicEq Double where
  isEqual lhs rhs = abs (lhs - rhs) < 0.00001

-- P/192
-- we can add new instances anywhere; they are not confined to
-- the module where we define a typeclass
-- this feature of the typeclass system is referred to as its
-- open world assumption

main :: IO ()
main = do
  -- print $ isEqual False True
  -- print $ superEqual True False
  print $ isEqual (3.14159265 :: Double) (3.14159261 :: Double)
  print $ isEqual (3 :: Double) (3.13141234 :: Double)
