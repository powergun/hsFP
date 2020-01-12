module FirstPrinciples.IdentityTMonad (demo) where

{-
here is the "reference" implementation, taken from earlier notes
(Haskell Cookbook)

instance Monad m => Monad (StateT s m) where
  return = pure
  sta >>= f =
    let newStateFunc s =
            let ma = runStateT sta s
            in  do
                  (a, s') <- ma
                  let stb = f a
                  runStateT stb s'
    in  StateT newStateFunc
-}

newtype IdentityT m a = IdentityT
  { runIdentityT :: m a }

instance (Functor m) => Functor (IdentityT m) where
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance (Applicative m) => Applicative (IdentityT m) where
  pure x = IdentityT (pure x)
  
  (IdentityT fab) <*> (IdentityT fa) = IdentityT (fab <*> fa)

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  m@(IdentityT ma) >>= f =
    -- impl 1
    -- IdentityT $ ma >>= runIdentityT . f -- `f a` produces `Identity m b`
    -- impl 2
    IdentityT $ runIdentityT . f =<< runIdentityT m

demo :: IO ()
demo = do
  putStrLn "//// IdentityT Rediscovered"
  print $ runIdentityT (IdentityT $ compute 111)
  where
    compute :: Int -> Either String Int
    compute n = do
      if n > 10
        then Left "too large"
        else Right n
