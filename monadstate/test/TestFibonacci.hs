-- source
-- haskell cookbook

module TestFibonacci
  ( runSpec
  )
where

import           Test.Hspec
import qualified Control.Monad.State           as S
import qualified Data.Map.Strict               as M
import           Fibonacci.ImplV6

runSpec :: IO ()
runSpec = hspec $ describe "Test Fibonacci" $ do
  it "demo execState() - it only returns the last state but not the result" $ do
    let mp     = S.execState (fibWithState 30) M.empty
        Just v = M.lookup 30 mp
    832040 `shouldBe` v

  it "demo evalState() - it returns the result and throws away the state" $ do
    let mp = S.execState (fibWithState 30) M.empty
    514229 `shouldBe` S.evalState (fibWithState 29) mp
    0 `shouldBe` S.evalState (fibWithState (-12)) mp

  it "demo runState() - it returns both the last state and the result" $ do
    let o = S.runState (fibWithState 15) M.empty
    610 `shouldBe` fst o
