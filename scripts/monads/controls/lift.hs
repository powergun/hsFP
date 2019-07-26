#!/usr/bin/env stack runghc

import Control.Monad

-- real world haskell P/267
-- liftM function
-- takes a regular function, concat and lifts it into the IO
-- monad. In other words, it takes the result of forM (of type 
-- IO [[Int]]) out of the IO monad, applies concat to it (yielding
-- a result of type [Int], which is what we need/print), and puts
-- the result back into the IO monad

-- returns a list of int wrapped in IO
demoLiftMForM :: IO [Int]
demoLiftMForM = do
  print "//// demo liftM in combination with forM"
  -- forM <pure value list> <action>
  liftM concat $ forM [1, 2, 3, 4, 5] $ \elem -> do
    return $ take elem (repeat elem)

demoLiftMSingleAction :: IO Int
demoLiftMSingleAction = do
  print "//// demo liftM with a single action"
  -- lift (+) 1 "plus one" into the IO monad
  -- takes the result from (return 2), which is 2, out of the IO
  -- monad and applies "plus one" to it, then puts the result back
  -- into the IO monad
  liftM ((+) 1) (return 2)

-- haskell cookbook L2692
-- the function liftM2 lifts a function to a monad and applies
-- it to two arguments
-- m a -> (a -> m b) -> m b
-- L2758
-- liftM2 takes a function with two args and two values of a data 
-- type for which the monad instance is defined. It lifts the 
-- function and applies to the monad
-- MY NOTES:
-- (*) list1 creates a new list that wraps partial functions;
-- this "list of partials", still in the form of "m a" is put
-- in front of <*> ...
-- see
-- https://stackoverflow.com/questions/53145952/implementing-liftm2-in-haskell
--

-- The general pattern is transforming

-- liftMn f a1 ... an

-- into

-- f <$> a1 <*> ... <*> an
-- -- i.e., more precisely
-- (... ((f <$> a1) <*> a2) ... <*> an)
demoLiftM2 :: IO ()
demoLiftM2 = do
  --                  row     column
  print $ liftM2 (*) [-1, 1] [3..6]

main :: IO ()
main = do
  demoLiftMForM >>= print
  demoLiftMSingleAction >>= print
  demoLiftM2
