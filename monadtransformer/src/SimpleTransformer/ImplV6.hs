module SimpleTransformer.ImplV6
  ( StateT(..)
  , get
  , put
  , lift
  )
where
data StateT s m a = StateT { runStateT :: s -> m (a, s) }
get :: Monad m => StateT s m s
get = StateT $ \s -> return (s, s)
put :: Monad m => s -> StateT s m ()
put s = StateT $ \_ -> return ((), s)
lift :: Monad m => m a -> StateT s m a
lift ma = StateT $ \s -> do
  anything <- ma
  return (anything, s)
instance Functor m => Functor (StateT s m) where
  fmap f sta =
    let newStateFunc s =
            let ma = runStateT sta s in fmap (\(a, s) -> (f a, s)) ma
    in  StateT newStateFunc
instance Applicative m => Applicative (StateT s m) where
  pure anything = StateT (\s -> pure (anything, s))
  stf <*> sta =
    let newStateFunc s =
            let mf = runStateT stf s
                ma = runStateT sta s
            in  (fmap (\(f, _) -> (\(a, s) -> (f a, s))) mf) <*> ma
    in  StateT newStateFunc
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
