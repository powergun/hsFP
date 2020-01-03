
import qualified Bifunctors
import qualified Contravariants
import qualified DeriveFunctor
import qualified NaturalTransformation
import qualified Structures

main :: IO ()
main = do
  Bifunctors.demo
  Contravariants.demo
  DeriveFunctor.demo
  Structures.demo
  NaturalTransformation.demo
