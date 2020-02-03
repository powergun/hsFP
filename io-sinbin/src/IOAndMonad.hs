module IOAndMonad (demo) where

import           Control.Monad (join)

demoMergeEffect :: IO ()
demoMergeEffect = do
  -- it won't compile without `join`
  -- in GHCi it won't print without `join`
  join . return $ print "iddqd"

demo :: IO ()
demo = do
  print "//// IO and Monad"
  demoMergeEffect
