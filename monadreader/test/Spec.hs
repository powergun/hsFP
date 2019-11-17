import qualified TestWrapHandler
import qualified TestWrapInteger
import qualified TestMoveCursor
import qualified TestWrapEnv

main :: IO ()
main = do
  TestWrapHandler.runSpec
  TestWrapInteger.runSpec
  TestMoveCursor.runSpec
  TestWrapEnv.runSpec

