#!/usr/bin/env stack runghc

import Control.Monad

-- real world haskell P/259
-- "check" function must be an action that returns IO Bool
-- like forM and mapM, elem is unwrapped from the IO wrapper
-- P/259:
-- filterM behaves like the normal filter function, but in this 
-- case it evaluates its predicate in the IO monad, allowing 
-- the predicate to perform IO
demoFilterM :: IO ()
demoFilterM = do
  (return [1, 2, 3, 4, 5]) >>= filterM check >>= print
  where
    check elem = return (elem `mod` 2 == 0)

-- programming haskell P173


main :: IO ()
main = do
  demoFilterM
