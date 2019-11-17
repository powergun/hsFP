import qualified TestCommands
import qualified TestMoveCursor
import qualified TestFibonacci
import qualified TestSimpleTransformer
import qualified TestSimpleState
import qualified TestGameState

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
  TestSimpleTransformer.runSpec
  TestSimpleState.runSpec
  TestGameState.runSpec
