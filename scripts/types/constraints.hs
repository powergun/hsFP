#!/usr/bin/env stack runghc

-- constraints
-- reduce the level of polymorphism, as it reduces the amount
-- of types at type-checking time, therefore allow function
-- to performance more operations on these values

-- programming haskell L1037
-- class constraint is written in the form C a
-- where C is the name of a class and a is a type variable
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- Numbers themselves are also overloaded: 
-- 3 :: Num a => a means for any numeric type a, the value 3 
-- has type a
-- in this manner the value 3 could be integer, float etc...
myFold :: (a -> b -> b) -> b -> [a] -> b
myFold _ b [] = b
myFold f b (a : as) = myFold f (f a b) as

-- mySum :: [a] -> a
-- No instance for (Num a) arising from a use of ‘+’
mySum :: Num a => [a] -> a
mySum = myFold (+) 0

main :: IO ()
main = print $ mySum [10, 20, 30]
