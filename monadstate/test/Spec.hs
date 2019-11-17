import qualified TestCommands
import qualified TestMoveCursor
import qualified TestFibonacci

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
