
import qualified FirstPrinciples.Bifunctors
import qualified Contravariants
import qualified DeriveFunctor
import qualified NaturalTransformation
import qualified Structures

main :: IO ()
main = do
  Contravariants.demo
  DeriveFunctor.demo
  Structures.demo
  NaturalTransformation.demo

  FirstPrinciples.Bifunctors.demo