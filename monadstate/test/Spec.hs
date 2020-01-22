import qualified FirstPrinciples.IsomorphismCheck
import qualified FirstPrinciples.Random
import qualified FirstPrinciples.RollDiceState
import qualified FirstPrinciples.StateFromScratch
import qualified TestCommands
import qualified TestFibonacci
import qualified TestGameState
import qualified TestMoveCursor
import qualified TestSimpleState

main :: IO ()
main = do
  TestCommands.runSpec
  TestMoveCursor.runSpec
  TestFibonacci.runSpec
  TestSimpleState.runSpec
  TestGameState.runSpec

  FirstPrinciples.Random.demo
  FirstPrinciples.IsomorphismCheck.demo
  FirstPrinciples.RollDiceState.demo
  FirstPrinciples.StateFromScratch.demo
