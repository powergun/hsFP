import qualified FirstPrinciples.IdentityTMonad
import qualified FirstPrinciples.MaybeTMonad
import qualified FirstPrinciples.TypeComposition
import qualified FirstPrinciples.EitherTMonad
import qualified FirstPrinciples.ReaderTMonad
import qualified TestDirWalker
import qualified TestFoldStopEarly
import qualified TestMaybeTrans
import qualified TestSimpleTransformer

main :: IO ()
main = do
    TestSimpleTransformer.runSpec
    TestDirWalker.demo
    TestMaybeTrans.demo
    TestFoldStopEarly.demo

    FirstPrinciples.TypeComposition.demo
    FirstPrinciples.IdentityTMonad.demo
    FirstPrinciples.MaybeTMonad.demo
    FirstPrinciples.EitherTMonad.demo
    FirstPrinciples.ReaderTMonad.demo

