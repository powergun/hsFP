import qualified TestTheComonad
import qualified TestTheProduct

main :: IO ()
main = do
  TestTheProduct.demo
  TestTheComonad.demo
