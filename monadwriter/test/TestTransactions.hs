module TestTransactions
  ( runSpec
  )
where

import           Test.Hspec
import           Transactions.ImplV3
import qualified Transactions.WriterImpl
import           Control.Monad.Writer           ( runWriterT )

runSpecWriterImpl :: IO ()
runSpecWriterImpl = do
  Transactions.WriterImpl.demo

runSpecWriterTransformerImpl :: IO ()
runSpecWriterTransformerImpl =
  hspec
    $ describe "Test transactions"
    $ it "given a list transactions, expect the resulting balance"
    $ do
        let transactions =
              [ Transaction (-10.0)
              , Transaction 5
              , Transaction 17
              , Transaction (-29)
              , Transaction 10
              ]
        (_, Transaction b) <- runWriterT (balanceSheet transactions)
        putStrLn $ "//// balance is " ++ show b
        b `shouldBe` (-7.0)

runSpec :: IO ()
runSpec = do
  runSpecWriterImpl
  runSpecWriterTransformerImpl
