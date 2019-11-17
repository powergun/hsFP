import qualified TestCommands
import qualified TestMoveCursor
import qualified TestFibonacci
import qualified TestSimpleTransformer

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
  TestSimpleTransformer.runSpec
