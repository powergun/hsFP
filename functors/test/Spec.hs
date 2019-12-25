
import qualified Bifunctors
import qualified Contravariants
import qualified DeriveFunctor

main :: IO ()
main = do
  Bifunctors.demo
  Contravariants.demo
  DeriveFunctor.demo
