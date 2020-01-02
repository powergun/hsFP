import qualified Mappend
import qualified MaybeMonoid
import qualified NonEmpty

main :: IO ()
main = do
  Mappend.demo
  MaybeMonoid.demo
  NonEmpty.demo
