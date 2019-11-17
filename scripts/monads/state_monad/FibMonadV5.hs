module FibMonadV5 
  (fibWithState) where
import qualified Control.Monad.State as Ms
import qualified Data.Map as M
type FibMap a = M.Map a a
type FibState a b = Ms.State (FibMap a) b
fibWithState :: Integral a => a -> FibState a a
fibWithState n | n <= 0 = return 0
               | n == 1 = return 1
               | otherwise = fibWithState' n
fibWithState' :: Integral a => a -> FibState a a
fibWithState' n = do
  cached <- get_ n
  case (cached) of
    Just result -> return result
    Nothing -> compute n
    where
      compute :: Integral a => a -> FibState a a
      compute n = do
        n_1 <- fibWithState (n - 1)
        n_2 <- fibWithState (n - 2)
        put_ n (n_1 + n_2)
get_ :: Integral a => a -> FibState a (Maybe a)
get_ n = do
  (M.lookup n) <$> Ms.get
put_ :: Integral a => a -> a -> FibState a a
put_ n result = do
  fibMap' <- (return (M.insert n result)) <*> Ms.get
  Ms.put fibMap'
  return result
