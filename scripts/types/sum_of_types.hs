#!/usr/bin/env stack runghc

-- similar to enum type in other languages

module ColourSimpleSumType (Colour) where
data Colour = Red | Green | Blue deriving Show
    --  type ctor   values

-- haskell design pattern P/58
-- type combination is also known as product of types
-- type alternation (e.g. maybe) as sum of types

-- real world haskell P/84
-- algebraic data types: can have more than one value ctor
-- such as data Bool = True | False
-- when a type has more than one value ctor, they are usually 
-- referred to as alternatives or cases
-- we can use any of the alternatives to create value of that type
type CardHolder = String
type CardNumber = String
type Address = [String]
type CustomerID = Int
data BillInfo = CreditCard CardNumber CardHolder Address
              | CashOnDelivery
              | Invoice CustomerID
  deriving (Show)

-- P/86
-- (unlike tuples) two algebaic types have distinct types even
-- if they are structurally equivalent

main :: IO ()
main = do
  print Red
