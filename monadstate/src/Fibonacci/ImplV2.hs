module Fibonacci.ImplV2
  ( fibWithState
  , getFib
  , putFib
  )
where

import qualified Control.Monad.State           as S
import qualified Data.Map.Strict               as M

-- keys and values are of the same type
type FibMap a = M.Map a a

-- state type, compute value type
type FibState a b = S.State (FibMap a) b

-- return a state and a Maybe value
getFib :: Integral a => a -> FibState a (Maybe a)
getFib n = M.lookup n <$> S.get

-- return a (modified) state and the given value
putFib :: Integral a => a -> a -> FibState a a
putFib n v = do
  fm <- (return (M.insert n v)) <*> S.get
  S.put fm
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
    n_1 <- getFibOr (n - 1)
    n_2 <- getFibOr (n - 2)
    putFib n (n_1 + n_2)
   where
    getFibOr n = do
      f <- getFib n
      case f of
        Just v  -> return v
        Nothing -> fibWithState' n
