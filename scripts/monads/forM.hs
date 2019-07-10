#!/usr/bin/env stack runghc

import Control.Monad

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook L2758
-- forM takes a traversable and applies a function (a -> m b)
-- to each of those elements to produce a traversable of b 
-- in the monad m
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- forM :: [a] -> (a -> m b) -> m [b]

-- real world haskell P/248
-- forM function acts a little like a for-loop: it maps its
-- second argument (an action) over its first (a list) and 
-- returns the list of results
-- ! intermediate value (in this demo, e) can be computed by 
-- ! pure functions, but must use let instead of <- (inpure)
demoForM :: IO ()
demoForM = do
  result <- forM [1, 2, 3, 4] $ \elem -> do
              -- ! elem is unwrapped from the IO wrapper
              -- from an opposite direction, return wraps the value
              -- in IO wrapper
              print $ "processing: " ++ (show elem)
              -- ! let can happen after IO action(s)
              let e = elem * 2 + 1
              -- return function wraps a value with the monad's
              -- type ctor
              return e
  print result
  
main :: IO ()
main = do
  demoForM
