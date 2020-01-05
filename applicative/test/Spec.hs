import qualified DemoQuickCheckApplicativeLaw
import qualified DemoQuickCheckMonoidLaw
import qualified FirstPrinciples.LiftA2ForBoolean
import qualified TestBasicsApFunctors
import qualified TestParserMonad
import qualified TestSingletonEnv
import qualified TestValidation

main :: IO ()
main = do
  TestParserMonad.runSpec
  TestBasicsApFunctors.demo
  TestValidation.demo
  TestSingletonEnv.demo
  DemoQuickCheckMonoidLaw.demo
  DemoQuickCheckApplicativeLaw.demo

  FirstPrinciples.LiftA2ForBoolean.demo
