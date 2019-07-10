#!/usr/bin/env stack runghc

-- real world haskell P/81
-- type ctor   value ctor (aka data ctor)
-- __________   ____
data BookInfo = Book Int String [String]
  deriving (Show)

-- type ctor: use it to refer to the new type; type name and 
-- hence a type ctor, must start with a capital letter
-- value ctor: use it to create a value of BookInfo

-- Int, String, [String] are components of the type

-- The BookInfo type contains the same components as a 3-tuple
-- of type (Int, String, [String]), but it has a distinct type.
-- We can't accidentally (or deliberately) use one in a context 
-- where the other is expected.

main :: IO ()
main = do
  print $ Book 1 "there" ["is", "acow"]
