#!/usr/bin/env stack runghc

-- L982
-- functions that take their arguments one a time are called
-- curried functions
-- useful functions can often be made by partially applying a 
-- curried functon with less than its full complement of 
-- arguments

curriedFunction :: String -> String -> String
curriedFunction x y = x ++ y
uncurriedFunction :: (String, String) -> String
uncurriedFunction (x, y) = x ++ y
demoUncurriedFunction :: IO ()
demoUncurriedFunction = do
    -- haskell design pattern P/48
    -- can create a partial function (borrowing partial from Python)
    let partial = curriedFunction "there is"
    print $ partial "a cow"
    -- can NOT create partial
    -- let partial = uncurriedFunction "there is"
    -- uncurried function consumes tuple

demoCurriedSum :: IO ()
demoCurriedSum = do
  print $ sum' 1 3
  where
      -- programming haskell L950, 
      -- curry: functions are free to return functions as results
      add' :: Int -> (Int -> Int)
      add' x y = x + y
      sum' x y = add' x y

oneArg :: Int -> Int
oneArg x = x
twoArgs :: Int -> Int -> Int
twoArgs x y = x + y
fourArgs :: Int -> Int -> Int -> Int -> Int
fourArgs x y z w = x + y + z + w 

demoCurryWithManyArgs :: IO ()
demoCurryWithManyArgs = do
  print $ func1 1
  print $ func2 2
  print $ func3 1 1 1
  print $ (func5 3) 1 1
  where
    -- all of these return another function!!
    func1 = (10 `twoArgs`) -- parentesses here are mandatory
                           -- otherwise it causes syntax error
    func2 a = a `twoArgs` 10 -- I can not use the pointless form
                             -- here because a is not the last arg
    func3 = (10 `fourArgs`) -- prefill the first arg
    -- func4 = (10 20 `fourArgs`) -- can not do that!
    func5 a = a `fourArgs` 10

main :: IO ()
main = do
    demoUncurriedFunction
    demoCurriedSum
    demoCurryWithManyArgs

