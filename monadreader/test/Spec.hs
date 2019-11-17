import qualified TestWrapHandler
import qualified TestWrapInteger
import qualified TestMoveCursor

main :: IO ()
main = do
  TestWrapHandler.runSpec
  TestWrapInteger.runSpec
  TestMoveCursor.runSpec

