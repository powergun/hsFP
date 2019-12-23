import qualified TestParserMonad
import qualified TestBasicsApFunctors

main :: IO ()
main = do
  TestParserMonad.runSpec
  TestBasicsApFunctors.demo
