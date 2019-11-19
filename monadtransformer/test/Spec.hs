import qualified TestDirWalker
import qualified TestSimpleTransformer

main :: IO ()
main = do
    TestSimpleTransformer.runSpec
    TestDirWalker.demo

