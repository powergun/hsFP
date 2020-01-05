
import qualified DemoQuickCheckMonadLaw
import qualified Either.Bind
import qualified FirstPrinciples.FunctorInTermsOfMonad
import qualified FromScratch.Monad
import qualified Validation.EnsureValueInList

main :: IO ()
main = do
  Either.Bind.demo
  FromScratch.Monad.demo
  Validation.EnsureValueInList.demo

  FirstPrinciples.FunctorInTermsOfMonad.demo
  DemoQuickCheckMonadLaw.demo
