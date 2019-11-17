module StateMonadTransformerV4 
  ( StateT(..)
  , get
  , put
  , lift
  , runStateT 
  ) where

data StateT s m a = StateT { stateFunc :: s -> m (a, s) }

runStateT = stateFunc

get :: Monad m => StateT s m s
get = 
  let newStateFunc s = pure (s, s)
  in StateT { stateFunc = newStateFunc }

put :: Monad m => s -> StateT s m ()
put s = 
  let newStateFunc _ = pure ((), s)
  in StateT { stateFunc = newStateFunc }

lift :: Monad m => m a -> StateT s m a
lift ma = 
  let newStateFunc s = do
        a <- ma
        return (a, s)
  in StateT { stateFunc = newStateFunc }

instance Monad m => Functor (StateT s m) where
  fmap f sta =
    let newStateFunc s =
          let t = \(a, s) -> (f a, s)
              ma = stateFunc sta s
          in t <$> ma
    in StateT { stateFunc = newStateFunc }

instance Monad m => Applicative (StateT s m) where
  pure x = 
    let newStateFunc s = pure (x, s)
    in StateT { stateFunc = newStateFunc }
  stf <*> sta = 
    let newStateFunc s = 
          let mf = stateFunc stf s
              ma = stateFunc sta s
              t (fab, _) = \(a, s) -> (fab a, s)
          in (t <$> mf) <*> ma
    in StateT { stateFunc = newStateFunc }

instance Monad m => Monad (StateT s m) where
  return = pure
  sta >>= f =
    let newStateFunc s = 
          let ma = stateFunc sta s
          in do
                (a, s') <- ma 
                stateFunc (f a) s'
    in StateT { stateFunc = newStateFunc }
