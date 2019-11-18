module TestSimpleTransformer
  ( runSpec
  )
where

-- source
-- haskell cookbook

import           Test.Hspec
import qualified SimpleTransformer.ImplV6      as STI

-- embed IO monad in the state transformer
-- get the current state and modify it with the supplied argument
-- recall:
-- all actions are
-- performed in the embedded monad, whereas the state transformer
-- is responsible for keeping state
modifyState :: Int -> STI.StateT Int IO ()
modifyState newValue = do
  STI.lift $ putStrLn $ "new state value " ++ (show newValue)
  i <- STI.get
  STI.lift $ putStrLn $ "state before " ++ (show i)
  STI.put newValue
  i' <- STI.get
  STI.lift $ putStrLn $ "state after  " ++ (show i')

runSpec :: IO ()
runSpec =
  hspec $ describe "Test SimpleTransformer" $ it "transform the state" $ do
    (_, state) <- STI.runStateT (modifyState 10) 100
    state `shouldBe` 10
    (_, state') <- STI.runStateT (modifyState 1234) 12
    state' `shouldBe` 1234


