import           Test.Hspec
import           WrapHandle

runSpec :: IO ()
runSpec = hspec $ do
  describe "Test WrapHandle" $ do
    it "write to file" $ do
      WrapHandle.demo

main :: IO ()
main = do
  runSpec
