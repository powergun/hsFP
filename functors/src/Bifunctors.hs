module Bifunctors where

import qualified Data.Bifunctor as DB

data GameSession = Either Int Int

demo :: IO ()
demo = do
  return ()
