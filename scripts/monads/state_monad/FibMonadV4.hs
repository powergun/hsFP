module FibMonadV4 
  ( fibWithState
  ) where

import qualified Control.Monad.State as Ms
import qualified Data.Map as M

data FibMap = M.Map a a
data FibState a b = Ms.State (FibMap a) b

fibWithState :: Integral a -> Ms.State a a