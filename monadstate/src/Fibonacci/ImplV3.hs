module Fibonacci.ImplV3
  ( fibWithState
  )
where

import qualified Control.Monad.State           as S
import qualified Data.Map                      as M

type FibMap a = M.Map a a
type FibState a b = S.State (FibMap a) b

getFib :: Integral a => a -> FibState a (Maybe a)
getFib n = (M.lookup n) <$> S.get

putFib :: Integral a => a -> a -> FibState a a
putFib n v = do
  newFm <- (return (M.insert n v)) <*> S.get
  S.put newFm
  return v

fibWithState :: Integral a => a -> FibState a a
fibWithState n = case n < 0 of
  True  -> fibWithState' 0
  False -> fibWithState' n

fibWithState' :: Integral a => a -> FibState a a
fibWithState' n = case n > 1 of
  False -> do
    f <- getFib n
    case f of
      Just v  -> return v
      Nothing -> putFib n n
  True -> do
    n_1 <- getFibOrCompute (n - 1)
    n_2 <- getFibOrCompute (n - 2)
    putFib n (n_1 + n_2)
   where
    getFibOrCompute n = do
      f <- getFib n
      case f of
        Just v  -> return v
        Nothing -> fibWithState' n
