import           Test.Hspec
import qualified TestWrapHandler
import qualified TestMoveCursor

main :: IO ()
main = do
  TestWrapHandler.runSpec
  TestMoveCursor.runSpec

