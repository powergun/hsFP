#!/usr/bin/env stack runghc

-- programming haskell L2301
-- a function that takes a function as an argument or returns
-- a function as a result is called a higher order function
-- the term higher order is often just used for taking functions
-- as arguments

-- haskell design pattern P/84
-- high order function - HOF

demoAllAny :: IO ()
demoAllAny = do
    print "demo all-any"
    print $ all even [1, 23,3 ,2, 1]
    print $ all odd [3, 4, 1, 1]
    print $ any even [1, 32,3 ,1,1]

demoTakeDropWhile :: IO ()
demoTakeDropWhile = do
    print "demo take-drop-while"
    -- while true (take stops at element-2, 21)
    print $ takeWhile even [32,32,21,312,1 ,1]
    -- while true (drop stops at element-1, 23)
    print $ dropWhile even [34,23,1,1,12,1,11,2]

main :: IO ()
main = do
    demoAllAny
    demoTakeDropWhile
