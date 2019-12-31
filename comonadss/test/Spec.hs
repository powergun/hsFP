import qualified TestComonadLib
import qualified TestTheComonad
import qualified TestTheProduct
import qualified TestTheStream

main :: IO ()
main = do
  TestTheProduct.demo
  TestTheComonad.demo
  TestComonadLib.demo
  TestTheStream.demo
