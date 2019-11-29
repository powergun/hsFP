{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Parametric.Demo where

import Data.Bool (bool)
import Data.Char (toUpper)
import           Control.Monad.Reader


data Options = Options {
  capitalize :: Bool
}

type AppConfig = MonadReader Options

process :: AppConfig m => String -> m String
process s = bool s (map toUpper s) <$> asks capitalize

{-
-- type: (MonadReader Options Maybe)
demo :: Maybe ()
demo = do
  s <- process "asd"
  return ()

-- type: (MonadReader Options IO)
demo :: IO ()
....
-}

