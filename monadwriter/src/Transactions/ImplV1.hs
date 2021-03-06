module Transactions.ImplV1
  ( Transaction(..)
  , printTransaction
  , balanceSheet
  )
where

import           Data.Monoid
import           Control.Monad.Writer

-- haskell cookbook L3482, 3530

-- L3530
-- a writer monad works with an assumption that the writer state 
-- is an instance of monoid. As a monoid you will get two properties
-- mempty: scratch value
-- mappend: combine two values
-- see also: monoid.hs

-- a writer monad hence starts with an empty (or scratch) value
-- As we use the function tell() the context of Writer monad,
-- the Writer monad keeps combining the existing value with a new
-- value and updates the writer state

newtype Transaction = Transaction Double deriving Show

-- how to resolve "No instance for (Semigroup...)" error
-- see
-- https://stackoverflow.com/questions/53622428/a-basic-monoid-definition-gives-no-instance-for-semigroup-mymonoid-arising-fr
instance Semigroup Transaction where
  (Transaction x) <> (Transaction y) = Transaction (x + y)

instance Monoid Transaction where
  mempty  = Transaction 0
  mappend = (<>)

-- to classify the transaction
printTransaction :: Transaction -> IO ()
printTransaction (Transaction x) | x < 0 = putStrLn $ "debiting " ++ (show x)
                                 | x > 0 = putStrLn $ "crediting " ++ (show x)
                                 | otherwise = putStrLn "no change"

-- given a list of transactions, write a function to keep
-- balancing using the Writer transformer
balanceSheet :: [Transaction] -> WriterT Transaction IO ()
balanceSheet []       = lift $ putStrLn "finished balancing"
balanceSheet (b : bs) = do
  tell b
  lift $ printTransaction b
  balanceSheet bs
