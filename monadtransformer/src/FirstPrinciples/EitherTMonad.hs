module FirstPrinciples.EitherTMonad
  ( demo
  )
where

import           Data.Bool                      ( bool )
import           Data.Either                    ( either )
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

-- the cheating method
swapEitherT' :: (Monad m) => EitherT e m a -> EitherT a m e
swapEitherT' (EitherT mea) = EitherT $ do
  ea <- mea
  case ea of
    Left  l -> return . Right $ l
    Right r -> return . Left $ r

swapEitherT :: (Functor m) => EitherT e m a -> EitherT a m e
swapEitherT (EitherT mea) =
  let swapEither :: Either e a -> Either a e
      swapEither (Left  l) = Right l
      swapEither (Right r) = Left r
  in  EitherT $ fmap swapEither mea

-- recall either() from Data.Either
eitherT :: Monad m => (a -> m c) -> (b -> m c) -> EitherT a m b -> m c
eitherT f g (EitherT mea) = mea >>= \ea -> either f g ea

demoFunctor :: IO ()
demoFunctor = do
  let m  = EitherT [Right 12] :: EitherT String [] Int
      m' = EitherT [Left "dead functor"] :: EitherT String [] Int
  print $ runEitherT (fmap (+ (-100)) m)
  print $ runEitherT (fmap (+ (-100)) m')

demoApplicative :: IO ()
demoApplicative = do
  let m  = pure 12 :: EitherT String [] Int
      mf = pure (+ (-100)) :: EitherT String [] (Int -> Int)
      m' = EitherT [Left "dead applicative"] :: EitherT String [] Int
  print $ runEitherT m
  print $ runEitherT (mf <*> m)
  print $ runEitherT (mf <*> m')

demoMonad :: IO ()
demoMonad = do
  let m = return 12 :: EitherT String [] Int
      f :: Int -> EitherT String [] Int
      f x = bool (EitherT [Left "dead monad"]) (return (x - 10)) (x > 0)
  print $ runEitherT (m >>= f)
  print $ runEitherT (m >>= f >>= f)
  print $ runEitherT (m >>= f >>= f >>= f)

demoSwapEitherT :: IO ()
demoSwapEitherT = do
  let m   = return 12 :: EitherT String [] Int
      m'  = swapEitherT m -- .. Int [] String
      m'' = swapEitherT' m
      e   = EitherT [Left "to swap"] :: EitherT String [] Int
      e'  = swapEitherT e
      e'' = swapEitherT e
  print (runEitherT m, runEitherT m', runEitherT e, runEitherT e')
  print (runEitherT m, runEitherT m'', runEitherT e, runEitherT e'')

demoEitherTCatamorphism :: IO ()
demoEitherTCatamorphism = do
  let m = return 12 :: EitherT String [] Int
      e = EitherT [Left "to catamorphism"] :: EitherT String [] Int
  print $ eitherT (const ["iddqd"]) (\x -> [show x]) m
  print $ eitherT (const ["iddqd"]) (\x -> [show x]) e

demo :: IO ()
demo = do
  print "//// EitherT rediscovered"
  demoFunctor
  demoApplicative
  demoMonad
  demoSwapEitherT
  demoEitherTCatamorphism
