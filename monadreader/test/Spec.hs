import qualified TestLocalSimple
import qualified TestMoveCursor
import qualified TestWrapEnv
import qualified TestWrapHandler
import qualified TestWrapInteger
import qualified FirstPrinciples.Reader
import qualified FirstPrinciples.ReaderPractice

main :: IO ()
main = do
  TestWrapHandler.runSpec
  TestWrapInteger.runSpec
  TestMoveCursor.runSpec
  TestWrapEnv.runSpec
  TestLocalSimple.demo

  FirstPrinciples.Reader.demo
  FirstPrinciples.ReaderPractice.main
