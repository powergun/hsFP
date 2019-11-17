module Fibonacci.ImplV1
  ( getFib
  , putFib
  , fibWithState
  )
where

-- haskell cookbook L3349

import qualified Control.Monad.State           as S
import qualified Data.Map.Strict               as M

type FibMap a = M.Map a a
-- recall: State s a
type FibState a b = S.State (FibMap a) b

getFib :: Integral a => a -> FibState a (Maybe a)
getFib i = M.lookup i <$> S.get

-- putFib <key> <value>
putFib :: Integral a => a -> a -> FibState a a
putFib i v = do
  mp <- (pure (M.insert i v)) <*> S.get
  S.put mp
  return v

fibWithState :: Integral a => a -> FibState a a
fibWithState n = case (n > 0) of
  True  -> fibWithState' n
  False -> fibWithState' 0

fibWithState' :: Integral a => a -> FibState a a
fibWithState' n = case (n > 1) of
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
    getFibOr m = do
      fm <- getFib m
      case fm of
        Just fv -> return fv
        Nothing -> fibWithState m
