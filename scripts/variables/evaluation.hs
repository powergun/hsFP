#!/usr/bin/env stack runghc

-- real world haskell P/72
-- In a language that uses strict evaluation, the arguments to 
-- a function are evaluated before the function is applied. 
-- Haskell chooses another path: nonstrict evaluation

-- In Haskell, the subexpression 1 + 2 is not reduced to the 
-- value 3. Instead, we create a “promise that when the value of 
-- the expression isOdd (1 + 2)isneeded,we’llbeable to compute it. 

-- The record that we use to track an unevaluated expression is 
-- referred to as a thunk.

main :: IO ()
main = do
  -- P/75
  -- Finally, we return from our original application, substituting 
  -- the result of the first recursive application:
  -- Notice that as we return from each successive recursive application,
  -- none of them needs to evaluate the expression tail "bcd" 
  -- the final result of evaluating the original expression is a 
  -- thunk. The thunk is only evaluated when ghci needs to print it.
  print $ drop 1 "abcd" == tail "abcd"
  print $ drop 2 "abcd" == tail "bcd"

