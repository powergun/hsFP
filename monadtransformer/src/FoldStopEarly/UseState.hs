module FoldStopEarly.UseState (foldState) where

import           Control.Monad       (mapM_)
import           Control.Monad.State (execState, modify')

-- NOTE this version does not do early-termination at all

foldState :: (b -> a -> b) -> b -> [a] -> b
foldState f accum0 list0 =
  execState (mapM_ go list0) accum0
  where
    go x = modify' (\accum -> f accum x)
