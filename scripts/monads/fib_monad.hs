#!/usr/bin/env stack runghc

import Control.Applicative
import qualified Control.Monad.State as S
import qualified Data.Map.Strict as M

type FibMap a = M.Map a a
-- recall: State s a
type FibState a b = S.State (FibMap a) b

getFib :: Integral a => a -> FibState a (Maybe a)
getFib i = M.lookup i <$> S.get

putFib :: Integral a => a -> a -> FibState a a
putFib i v = do
  mp <- (pure (M.insert i v)) <*> S.get
  put mp
  return v

main :: IO () 
main = do
  print $ putFib 1 2
