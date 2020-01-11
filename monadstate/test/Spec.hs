import qualified TestCommands
import qualified TestMoveCursor
import qualified TestFibonacci
import qualified TestSimpleState
import qualified TestGameState
import qualified FirstPrinciples.Random

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
  TestSimpleState.runSpec
  TestGameState.runSpec

  FirstPrinciples.Random.demo
