import qualified TestBasicsApFunctors
import qualified TestParserMonad
import qualified TestValidation

main :: IO ()
main = do
  TestParserMonad.runSpec
  TestBasicsApFunctors.demo
  TestValidation.demo
