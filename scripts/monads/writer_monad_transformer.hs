#!/usr/bin/env stack runghc

-- haskell cookbook L3482, 3530

import Data.Monoid
import Control.Monad.Writer

-- L3530
-- a writer monad works with an assumption that the writer state 
-- is an instance of monoid. As a monoid you will get two properties
-- mempty: scratch value
-- mappend: combine two values
-- see also: monoid.hs

newtype Transaction = Transaction Double deriving Show

-- how to resolve "No instance for (Semigroup...)" error
-- see
-- https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
instance Semigroup Transaction where
  (Transaction x) <> (Transaction y) = Transaction (x + y)

instance Monoid Transaction where
  mempty = Transaction 0
  mappend = (<>)

-- to classify the transaction
printTransaction :: Transaction -> IO ()
printTransaction (Transaction x) 
                 | x < 0 = putStrLn $ "debiting " ++ (show x)
                 | x > 0 = putStrLn $ "crediting " ++ (show x)
                 | otherwise = putStrLn "no change"

-- given a list of transactions, write a function to keep
-- balancing using the Writer transformer
balanceSheet :: [Transaction] -> WriterT Transaction IO ()
balanceSheet [] =
  lift $ putStrLn "finished balancing"
balanceSheet (b:bs) = do
  tell b
  lift $ printTransaction b
  balanceSheet bs

demoTransactions :: IO ()
demoTransactions = do
  let transactions = [ Transaction (-10.0)
                     , Transaction 5
                     , Transaction 17
                     , Transaction (-29)
                     , Transaction 10]
  (_, Transaction b) <- runWriterT (balanceSheet transactions)
  putStrLn $ "balance is " ++ (show b)

main :: IO ()
main = do
  demoTransactions

