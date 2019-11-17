module TestWrapHandler
  ( runSpec
  )
where

import           Test.Hspec
import           WrapHandler.ImplV1

runSpec :: IO ()
runSpec = hspec $ describe "Test WrapHandle" $ it
  "write to file, then read its contents"
  WrapHandler.ImplV1.demo
