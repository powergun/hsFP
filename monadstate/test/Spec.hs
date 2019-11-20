import qualified TestCommands
import qualified TestMoveCursor
import qualified TestFibonacci
import qualified TestSimpleState
import qualified TestGameState

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
  TestSimpleState.runSpec
  TestGameState.runSpec
