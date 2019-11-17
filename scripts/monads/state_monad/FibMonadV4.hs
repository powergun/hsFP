module FibMonadV4 
  ( fibWithState
  ) where

import qualified Control.Monad.State as Ms
import qualified Data.Map as M

type FibMap a = M.Map a a
type FibState a b = Ms.State (FibMap a) b

fibWithState :: Integral a => a -> FibState a a
fibWithState n 
  | n <= 0    = return 0
  | n == 1    = return 1
  | otherwise = fibWithState' n

get :: Integral a => a -> FibState a (Maybe a)
get key = do
  (M.lookup key) <$> Ms.get

put :: Integral a => a -> a -> FibState a a
put key value = do
  newMap <- (return (M.insert key value)) <*> Ms.get
  Ms.put newMap
  return value

fibWithState' :: Integral a => a -> FibState a a
fibWithState' n = do
  cached <- get n
  case (cached) of
    Just m -> return m
    Nothing -> do
      n_1 <- getOrCompute (n - 1)
      n_2 <- getOrCompute (n - 2)
      put n (n_1 + n_2)
      where
        getOrCompute n = do
          cached' <- get n
          case (cached') of
            Just m' -> return m'
            Nothing -> fibWithState n
