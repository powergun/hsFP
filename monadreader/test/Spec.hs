import           Test.Hspec
import           WrapHandle
import           WrapInteger

runSpec :: IO ()
runSpec = hspec $ do
  describe "Test WrapHandle" $ do
    it "write to file, then read its contents" $ do
      WrapHandle.demo

  describe "Test WrapInteger" $ do
    it "access the environment (a single integer)" $ do
      WrapInteger.demo

main :: IO ()
main = do
  runSpec
