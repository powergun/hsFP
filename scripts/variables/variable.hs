#!/usr/bin/env stack runghc

-- real world haskell P/69
-- a variable provides a way to give a name to an expr
-- once a variable is bound to a particular expr, its value
-- does not change: we can always use the name of the variable
-- instead of writing out the expr, and we will get the same 
-- result either way

-- in haskell once we've bound a variable to an expr, we know 
-- that we can always substitute it for that expr, because 
-- it will not change. In an imperative language, this notion
-- of substitutability does not hold

main :: IO ()
main = do
  print 1
