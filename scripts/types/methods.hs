#!/usr/bin/env stack runghc

-- first seen in real world haskell P/284 "runParse"
-- NOTE: use a simplified demo to understand the book example

-- this is a prarameterized type; it wraps a function and provides 
-- an accessor for it; it works slightly like a method in OO
-- the "method" takes an Int (the ParseState in the book example)
-- either produces some value for further parsing or flag the 
-- caller to stop because parsing has failed
-- NOTE: using newtype and data has no difference in functionality
-- see why: https://wiki.haskell.org/Newtype
-- So if you want to declare different type class instances for 
-- a particular type, or want to make a type abstract, you can 
--                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- wrap it in a newtype and it'll be considered distinct to the 
-- type-checker, but identical at runtime.
newtype Parse a = Parse {
  runParse :: Int -> Either String (a, Int)
}

-- this is the specialized type that (only) works with String
-- the return value is hardcoded in the Either value; useless
-- to help me understand this: think of it as an object that 
-- has a method;
-- to call the method, go (runParse returnHardcodedString) ...
returnHardcodedString :: Parse String
returnHardcodedString =
  Parse $ \x -> 
    case x > 0 of
      False -> Left "fail"
      True -> Right ("asd", x)

-- this is rather awkward and leak the implementation detail (
-- the name of the accessor, runParse); see demoVirtualMethod
-- for how to design this by contract
demoCallMethod :: IO ()
demoCallMethod = do
  print "//// demo call method"
  print $ (runParse returnHardcodedString) 1
  print $ (runParse returnHardcodedString) (-1)

-- if the above abstract type notion makes sense, this is to 
-- design the collaborator (of Parse type) so that it encapsulates
-- the detail of Parse but still allow the client to extend 
-- its functionality by implementing new specialized Parse types
parse :: Parse a -> Int -> Either String a
parse parser initState
  = case runParse parser initState of
      Left err -> Left err
      Right (result, _) -> Right result

demoVirtualMethod :: IO ()
demoVirtualMethod = do
  print "//// demo design by contract - virtual method"
  print $ parse returnHardcodedString 1
  print $ parse returnHardcodedString (-10)

mustGreaterThan10 :: Parse String
mustGreaterThan10 =
  Parse $ \x ->
    case x > 10 of
      True -> Right ("SUCCESS", x)
      False -> Left "fail (! >10)"

mustGreaterThan100 :: Parse String
mustGreaterThan100 =
  Parse $ \x ->
    case x > 100 of
      True -> Right ("SUCCESS", x)
      False -> Left "fail (! >100)"

-- the goal of this combinator is to produce a new Parse value
-- using two existing Parse values
-- NOTE: this is different to real world haskell P/283 example
-- where the second arg is a "parser generator" because of its 
-- getState, putState design
(==>) :: Parse a -> Parse a -> Parse a
firstParser ==> secondParser =
  Parse chainedParser
  where
    chainedParser x =
      case runParse firstParser x of
        Left err -> Left err
        Right (_, newState) -> runParse secondParser newState

demoChainVirtualMethod :: IO ()
demoChainVirtualMethod = do
  print "//// demo chaining virtual method (custom operator)"
  -- not how the chain of parsing terminates early if the first
  -- parser fails 
  print $ parse (mustGreaterThan10 ==> mustGreaterThan100) 999
  print $ parse (mustGreaterThan10 ==> mustGreaterThan100) 99
  print $ parse (mustGreaterThan10 ==> mustGreaterThan100) 1

main :: IO ()
main = do
  demoCallMethod
  demoVirtualMethod
  demoChainVirtualMethod

