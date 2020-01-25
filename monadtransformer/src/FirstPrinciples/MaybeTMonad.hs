module FirstPrinciples.MaybeTMonad (demo) where

import           Control.Applicative (liftA, liftA2)
import           Data.Bool           (bool)

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Functor m) => Functor (MaybeT m) where
  fmap f mbma = MaybeT $ (fmap . fmap) f (runMaybeT mbma)

instance (Applicative m) => Applicative (MaybeT m) where
  pure x = MaybeT $ pure (pure x)
  -- m (Maybe (a -> b))   m (Maybe a)
  --    Maybe (a -> b) <*> Maybe a
  (<*>) (MaybeT mmbfab) (MaybeT mmba) =
    MaybeT $ (<*>) <$> mmbfab <*> mmba

instance (Monad m) => Monad (MaybeT m) where
  return = pure

  (MaybeT mmba) >>= f = MaybeT $ do -- inside MaybeT
                          mba <- mmba -- extract `Maybe a` from `m (Maybe a)`
                          case mba of
                            Nothing -> return Nothing -- m (Maybe a)
                            Just x  -> runMaybeT (f x) -- m (Maybe a)



demoFunctor :: IO ()
demoFunctor = do
  let m :: MaybeT [ ] Int
      m = return 1
      f = (+ 10)
      m' = fmap f m
      m'' = MaybeT [Nothing]
  print $ runMaybeT m'
  print $ runMaybeT (fmap f m'')

demoApplicative :: IO ()
demoApplicative = do
  let af :: MaybeT [ ] (Int -> Int -> Int)
      af = return (+)
      a1 :: MaybeT [ ] Int
      a1 = MaybeT [Nothing]
      a2 :: MaybeT [ ] Int
      a2 = return 1
      a3 :: MaybeT [ ] Int
      a3 = return 1000
  print $ runMaybeT (af <*> a2 <*> a3)
  -- keep only the effect not the value
  print $ runMaybeT (af <* a1 <*> a2 <*> a3)

demoMonad :: IO ()
demoMonad = do
  let m = (return 1) :: MaybeT [ ] Int
      m' = (return (-1)) :: MaybeT [ ] Int
      f :: Int -> MaybeT [ ] Int
      f x = bool (MaybeT [Nothing]) (return . (* 1000) $ x) (x > 0)
  print $ runMaybeT (m >>= (\x -> return (x - 10)) >>= f)
  print $ runMaybeT (m' >>= (\x -> return (x + 10)) >>= f)

demo :: IO ()
demo = do
  print "//// MaybeT rediscovered"
  demoFunctor
  demoApplicative
  demoMonad
