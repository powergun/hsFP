module TestWrapInteger
  ( runSpec
  )
where

import           Test.Hspec
import           WrapInteger.ImplV1

runSpec :: IO ()
runSpec = hspec $
  describe "Test WrapInteger" $
    it "access to shared state, which is a single integer" $ do
      WrapInteger.ImplV1.demo
      WrapInteger.ImplV1.demo2
