module TestFoldStopEarly (demo) where

import           Test.Hspec

import qualified FoldStopEarly.UseEither
import qualified FoldStopEarly.UseEitherT
import qualified FoldStopEarly.UseState

demoTerminateEarly :: IO ()
demoTerminateEarly = do
  hspec $ describe "Test fold-terminate-early" $
    it "return the result when elem is 0" $ do
      let p :: Int -> Int -> Either Int Int
          p accum elem = case elem == 0 of
                          True  -> Left accum
                          False -> Right (accum + elem)
          ret = FoldStopEarly.UseEither.foldTerminate p 0 [1, 2, 0, 3, 4]
      ret `shouldBe` 3

  hspec $ describe "Test fold-state (not to terminate)" $
    it "iterate over the full list" $ do
      let p :: Int -> Int -> Int
          p accum x = accum + x
          ret = FoldStopEarly.UseState.foldState p 0 [1, 2, 0, 3, 4]
      ret `shouldBe` 10

  hspec $ describe "Test fold-either-transformer (ExceptT)" $
    it "return the result when elem is 0" $ do
      shouldBe 3 =<< FoldStopEarly.UseEitherT.demo 0 [1, 2, 0, 3, 4]
      shouldBe 10 =<< FoldStopEarly.UseEitherT.demo 0 [1, 2, 3, 4]
      shouldBe 10 =<< FoldStopEarly.UseEitherT.demo 10 []

demo :: IO ()
demo = do
  demoTerminateEarly
