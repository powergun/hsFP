#!/usr/bin/env stack runghc

-- programming haskell L1425,
-- sections

-- (#) = \x -> (\y -> x # y)
-- (x #) = \y -> x # y
-- (# y) = \x -> x # y

main :: IO ()
main = do
    print $ sumAll [1, 2, 3, 4]
    where 
        sumAll :: [Int] -> Int
        sumAll = foldl (+) 0
