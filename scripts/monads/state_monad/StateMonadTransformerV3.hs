module StateMonadTransformerV3
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
        pure (a, s)
  in StateT { stateFunc = newStateFunc }

instance Monad m => Functor (StateT s m) where
  fmap f sta =
    let newStateFunc s =
          let t = \(a, s) -> (f a, s)
              ma = stateFunc sta s
          in t <$> ma
    in StateT { stateFunc = newStateFunc }

instance Monad m => Applicative (StateT s m) where
  -- x is substituted to (a, s) 
  -- in the definition of lift()
  pure x = StateT { stateFunc = (\s -> pure (x, s)) }
  stf <*> sta = 
    let newStateFunc s = 
          -- the outer monad is to manage and provide the state
          -- to the inner monad, therefore the same s is passed
          -- to both stateFunc
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
   