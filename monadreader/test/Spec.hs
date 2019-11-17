import           Test.Hspec

runSpec :: IO ()
runSpec = hspec $ do
  describe "Test nothing" $ do
    it "placeholder" $ do
      1 `shouldBe` 1

main :: IO ()
main = do
  runSpec
