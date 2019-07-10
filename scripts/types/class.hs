#!/usr/bin/env stack runghc

-- a class defines the interface (or contract)
-- the data (structs) implement this interface to fulfill
-- the contract
-- this mechanism looks similar to rust's trait workflow

class Frobber a where
    frob :: a -> (String, Integer)

data A = A { aValue :: Int }
instance Frobber A where
    frob a = let value = aValue a in (show value, toInteger value)

data B = B { bValue :: Integer }
instance Frobber B where
    frob b = let value = bValue b in (show value, toInteger value)

data C = C { cValue :: Double }
instance Frobber C where
    frob c = let value = cValue c in (show value, round value)

printFrobResult :: Frobber a => a -> IO()
-- this function takes a value that fulfills the contract
-- of Frobber, regardless of the its implementation details
printFrobResult = print . frob
-- call the interface function

-- this is static polymorphism
-- recall the dynamic polymorphism explained in rust training
-- course - the one that requires vtable look up

main :: IO ()
main = do
    printFrobResult (A 100)
    printFrobResult (B (2 ^ 70))
    printFrobResult (C 3.1415926)
