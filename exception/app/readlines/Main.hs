module Main (main) where

import           BestPractices.Readlines
import           Control.Exception       (SomeException)

main :: IO ()
main = do
  putStrLn "Enter an Int (non-runtime exception)"
  res1 <- readLine1
  print (res1 :: Either SomeException Int)
  putStrLn "Enter an Int (runtime exception)"
  res2 <- readLine2
  print (res2 :: Int)
