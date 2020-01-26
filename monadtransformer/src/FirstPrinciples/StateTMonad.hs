module FirstPrinciples.StateTMonad
  ( demo
  )
where

-- import Data.Bifunctor (first)
import           Data.Bool                      ( bool )

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

instance (Functor m) => Functor (StateT s m) where
  fmap f (StateT smas) = StateT $ \s ->
    let mas = smas s
        t (a, s) = (f a, s)
        -- cheat mode: use first() from Data.Bifunctor (because
        -- a tuple is a Bifunctor) 
        -- mbs = fmap (first f) mas

        -- or handcraft the intermediate function t
        -- t :: (a, s) -> (b, s)
        -- fmap t :: m (a, s) -> m (b, s)
        mbs = fmap t mas
    in  mbs

instance (Applicative m) => Applicative (StateT s m) where
  pure x = StateT $ \s -> pure (x, s)

-- given t    (f, s)     (a, s)  =   (f a, s)
-- want: t (m (f, s)) (m (a, s)) = m (f a, s)
  (StateT smfs) <*> (StateT smas) = StateT $ \s ->
    let mfs = smfs s -- m (f, s)
        mas = smas s -- m (a, s)
        t (f, s) (a, _) = (f a, s)
    in  t <$> mfs <*> mas

instance (Monad m) => Monad (StateT s m) where
  return = pure

-- f :: a -> StateT s m a
  (StateT smas) >>= f = StateT $ \s -> do -- m (a s)
    (a, s') <- smas s
    runStateT (f a) s'

demoFunctor :: IO ()
demoFunctor = do
  let m  = StateT (\s -> [(10, s)]) :: StateT String [] Int
      mm = StateT (\s -> bool Nothing (Just (length s, "")) (not . null $ s))
      f  = (+ 1000)
  print $ runStateT (fmap f m) "iddqd"
  print $ runStateT (fmap f mm) "idkfa"
  print $ runStateT (fmap f mm) ""

demoApplicative :: IO ()
demoApplicative = do
  let ma  = pure 10 :: StateT String [] Int
      mf  = pure (+ 1000) :: StateT String [] (Int -> Int)
      ma' = StateT (const Nothing) :: StateT String Maybe Int
      ma3 = StateT (\s -> Just (1, s)) :: StateT String Maybe Int
      mf' = pure (+ 1000) :: StateT String Maybe (Int -> Int)
  print $ runStateT (mf <*> ma) "imp"
  print $ runStateT (mf' <*> ma') "imp alpha"
  print $ runStateT (mf' <*> ma3) "imp cyborg"

demoMonad :: IO ()
demoMonad = do
  let ma = return 10 :: StateT String Maybe Int
      mb = StateT (const Nothing) :: StateT String Maybe Int
      f :: Int -> StateT String Maybe Int
      f x = return (x + 1000)
  print $ runStateT (ma >>= f) "e1m1"
  print $ runStateT (mb >>= f) "e3m3"

demo :: IO ()
demo = do
  print "//// StateT rediscovered"
  demoFunctor
  demoApplicative
  demoMonad
