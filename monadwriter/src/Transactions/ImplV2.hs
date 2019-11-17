module Transactions.ImplV2
  ( Transaction(..)
  , printTransaction
  , balanceSheet
  )
where
import           Control.Monad.Writer
newtype Transaction = Transaction Double deriving (Show)
printTransaction (Transaction t) | t == 0 = putStrLn "no change"
                                 | t < 0  = putStrLn $ "debit: " ++ (show t)
                                 | t > 0  = putStrLn $ "credit: " ++ (show t)
balanceSheet :: [Transaction] -> WriterT Transaction IO ()
balanceSheet []       = lift $ putStrLn "end of transactions"
balanceSheet (b : bs) = do
  tell b
  lift $ printTransaction b
  balanceSheet bs
instance Semigroup Transaction where
  (<>) = mappend
instance Monoid Transaction where
  mempty = Transaction 0
  mappend (Transaction t1) (Transaction t2) = Transaction (t1 + t2)
