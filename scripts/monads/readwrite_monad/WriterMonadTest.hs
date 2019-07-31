#!/usr/bin/env stack runghc

import WriterMonadV3

import Control.Monad.Writer (runWriterT)

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

demoTransactions :: IO ()
demoTransactions = do
  let transactions = [ Transaction (-10.0)
                     , Transaction 5
                     , Transaction 17
                     , Transaction (-29)
                     , Transaction 10]
  (_, Transaction b) <- runWriterT (balanceSheet transactions)
  assert (b == -7.0)
  putStrLn $ "//// balance is " ++ (show b)

main :: IO ()
main = do
  demoTransactions

