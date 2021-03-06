import qualified FirstPrinciples.EitherTMonad
import qualified FirstPrinciples.Exercises
import qualified FirstPrinciples.ExtendStructure
import qualified FirstPrinciples.IdentityTMonad
import qualified FirstPrinciples.MaybeTMonad
import qualified FirstPrinciples.NoCommute
import qualified FirstPrinciples.ReaderTMonad
import qualified FirstPrinciples.RWSTMonad
import qualified FirstPrinciples.StateTMonad
import qualified FirstPrinciples.TypeComposition
import qualified FirstPrinciples.Unwrap
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
    FirstPrinciples.StateTMonad.demo
    FirstPrinciples.RWSTMonad.demo
    FirstPrinciples.Unwrap.demo
    FirstPrinciples.ExtendStructure.demo
    FirstPrinciples.NoCommute.demo
    FirstPrinciples.Exercises.demo
