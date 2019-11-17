module TestTransactions
  ( runSpec
  )
where

import           Test.Hspec
import           Transactions.ImplV3
import           Control.Monad.Writer           ( runWriterT )

runSpec :: IO ()
runSpec =
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
