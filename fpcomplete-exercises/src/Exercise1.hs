module Exercise1 (main) where

import           Control.Applicative (Applicative, (<*>))
import           Prelude             (Monad, return)
import qualified Prelude

fmap :: (Applicative m, Monad m) => (a -> b) -> (m a -> m b)
fmap f ma = return f <*> ma

main =
  case fmap (Prelude.+ 1) (Prelude.Just 2) of
    Prelude.Just 3 -> Prelude.putStrLn "Good job!"
    _              -> Prelude.putStrLn "Try again"
