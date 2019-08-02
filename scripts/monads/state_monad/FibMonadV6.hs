module FibMonadV6 (fibWithState) where
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
  cached_ <- get_ n
  case (cached_) of
    Just v -> return v
    Nothing -> compute n
compute :: Integral a => a -> FibState a a
compute n = do
  n_1 <- fibWithState (n - 1)
  n_2 <- fibWithState (n - 2)
  put_ n (n_1 + n_2)
get_ :: Integral a => a -> FibState a (Maybe a)
get_ n = do
  (M.lookup n) <$> Ms.get
put_ :: Integral a => a -> a -> FibState a a
put_ k v = do
  map_ <- (return $ M.insert k v) <*> Ms.get
  Ms.put map_
  return v