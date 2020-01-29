module FirstPrinciples.NoCommute (demo) where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

type A1 r = ReaderT r Maybe
type A2 r = MaybeT (Reader r)

-- env is [String]; return value is Int
computeA1 :: A1 [String] Int
computeA1 = do
  env <- ask
  return . length $ env

-- env is [String]; return value is Int
computeA2 :: A2 [String] Int
computeA2 = do
  env <- lift ask
  return . length $ env

demo :: IO ()
demo = do
  print "//// Monads do not commute"
  print $ runReaderT computeA1 ["asd"]

  print $ runReader (runMaybeT computeA2) ["asd"]
