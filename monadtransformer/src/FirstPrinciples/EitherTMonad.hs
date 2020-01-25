module FirstPrinciples.EitherTMonad
  ( demo
  )
where

import           Data.Bool                      ( bool )

newtype EitherT e m a =
  EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
  fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
  pure = EitherT . pure . pure
  (EitherT mef) <*> (EitherT mea) = EitherT $ (<*>) <$> mef <*> mea

instance (Monad m) => Monad (EitherT e m) where
  return = pure

  (EitherT mea) >>= f = EitherT $ do
    ea <- mea
    case ea of
      Left  l -> return . Left $ l
      Right r -> runEitherT (f r)


demoFunctor :: IO ()
demoFunctor = do
  let m  = EitherT [Right 12] :: EitherT String [] Int
      m' = EitherT [Left "dead"] :: EitherT String [] Int
  print $ runEitherT (fmap (+ (-100)) m)
  print $ runEitherT (fmap (+ (-100)) m')

demoApplicative :: IO ()
demoApplicative = do
  let m  = pure 12 :: EitherT String [] Int
      mf = pure (+ (-100)) :: EitherT String [] (Int -> Int)
      m' = EitherT [Left "dead"] :: EitherT String [] Int
  print $ runEitherT m
  print $ runEitherT (mf <*> m)
  print $ runEitherT (mf <*> m')

demoMonad :: IO ()
demoMonad = do
  let m = return 12 :: EitherT String [] Int
      f :: Int -> EitherT String [] Int
      f x = bool (EitherT [Left "dead"]) (return (x - 10)) (x > 0)
  print $ runEitherT (m >>= f)
  print $ runEitherT (m >>= f >>= f)
  print $ runEitherT (m >>= f >>= f >>= f)

demo :: IO ()
demo = do
  print "//// EitherT rediscovered"
  demoFunctor
  demoApplicative
  demoMonad
