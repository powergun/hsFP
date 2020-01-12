import qualified TestDirWalker
import qualified TestFoldStopEarly
import qualified TestMaybeTrans
import qualified TestSimpleTransformer
import qualified FirstPrinciples.TypeComposition

main :: IO ()
main = do
    TestSimpleTransformer.runSpec
    TestDirWalker.demo
    TestMaybeTrans.demo
    TestFoldStopEarly.demo

    FirstPrinciples.TypeComposition.demo
