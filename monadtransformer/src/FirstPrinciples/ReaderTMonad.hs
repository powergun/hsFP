module FirstPrinciples.ReaderTMonad
  ( demo
  )
where

import           Data.Bool                      ( bool )
import           Data.Char                      ( toUpper )

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
  -- f :: a -> b
  fmap f (ReaderT rma) = ReaderT $ fmap f . rma

instance (Applicative m) => Applicative (ReaderT r m) where
  pure x = ReaderT (\_ -> pure x)

  (ReaderT rmf) <*> (ReaderT rma) = ReaderT $ \r ->
    let f = rmf r
        a = rma r
    in  f <*> a

instance (Monad m) => Monad (ReaderT r m) where
  return = pure
--              f :: a -> ReaderT r m b
--              after fmap
--                 m a -> m (ReaderT r m b) 
  (ReaderT rma) >>= f = ReaderT $ \r -> do -- m a
    a <- rma r  -- ma >>= (\a -> ...)
    runReaderT (f a) r

demoFunctor :: IO ()
demoFunctor = do
  let m :: ReaderT [Int] [] Char
      m = ReaderT (\env -> bool "left" "right" (not . null $ env))
  print $ runReaderT (fmap toUpper m) [1, 2, 3]
  print $ runReaderT (fmap toUpper m) []

demoApplicative :: IO ()
demoApplicative = do
  let m :: ReaderT [Int] [] Char
      m = ReaderT $ const "iddqd"
      f :: ReaderT [Int] [] (Char -> Char)
      f = pure toUpper
  print $ runReaderT (f <*> m) [1, 2, 3]

demoMonad :: IO ()
demoMonad = do
  let m = return "iddqd idkfa" :: ReaderT [Int] [] String
  print $ runReaderT (m >>= (return . fmap toUpper)) [1, 2, 3]

demo :: IO ()
demo = do
  print "//// ReaderT rediscovered"
  demoFunctor
  demoApplicative
  demoMonad
