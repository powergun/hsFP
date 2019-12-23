
import qualified Either.Bind
import qualified FromScratch.Monad
import qualified Validation.EnsureValueInList

main :: IO ()
main = do
  Either.Bind.demo
  FromScratch.Monad.demo
  Validation.EnsureValueInList.demo
