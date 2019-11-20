import qualified TestDirWalker
import qualified TestSimpleTransformer
import qualified TestMaybeTrans

main :: IO ()
main = do
    TestSimpleTransformer.runSpec
    TestDirWalker.demo
    TestMaybeTrans.demo

