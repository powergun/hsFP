#!/usr/bin/env stack runghc

-- inspired by video Monad in less than 10 minutes

-- we can write code that is polymorphic in our choice of Monad

-- foldM works with list monad and maybe monad; I don't have to 
-- worry about the process of folding, but instead I can focus
-- on the lambda part that talks to the pure value
import Control.Monad
main :: IO ()
main = do
  r1 <- foldM (\s e -> return (s + e)) 0 [1, 3, 4, 5]
  print r1

  r2 <- foldM (\s e -> return (s ++ e)) "there" (Just "is")
  print r2
