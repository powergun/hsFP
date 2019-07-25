#!/usr/bin/env stack runghc

-- inspired by video Monad in less than 10 minutes
-- we can write code that is polymorphic in our choice of Monad
import Control.Monad
main :: IO ()
main = do
  result <- foldM (\s e -> return e) 0 [1]
  print result

