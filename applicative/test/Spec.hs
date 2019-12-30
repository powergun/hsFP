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
