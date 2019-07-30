module StateMonadTransformerV5 
  ( StateT(..)
  , get
  , put
  , lift
  ) where

data StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}

get :: (Monad m) => StateT s m s
get =
  let newStateFunc s = pure (s, s)
  in StateT { runStateT = newStateFunc }

put :: (Monad m) => s -> StateT s m ()
put s = 
  let newStateFunc _ = pure ((), s)
  in StateT { runStateT = newStateFunc }

lift :: (Monad m) => m a -> StateT s m a
lift ma = 
  let newStateFunc s = do
        -- the type of ma is unknown (ma is merely known to be
        -- a monad, and I can use <- or >>= operator with it)
        -- therefore I can not decompose "anything" to the tuple
        -- unlike in the definition of >>= 
        -- also recall that, what (<-) really does is to simplify 
        -- >>= 
        -- ma >>= (\anything -> ...)
        anything <- ma
        return (anything, s)
  in StateT { runStateT = newStateFunc }

instance Functor m => Functor (StateT s m) where
  fmap f sta = 
    let newStateFunc s = 
          let ma = runStateT sta s -- (a, s)
              t (a, s) = (f a, s)
          in t <$> ma
    in StateT { runStateT = newStateFunc }

instance Applicative m => Applicative (StateT s m) where
  pure x = 
    let newStateFunc s = pure (x, s)
    in StateT { runStateT = newStateFunc }
  stf <*> sta = 
    -- does not sequence the actions, hence same s
    let newStateFunc s = 
          let mf = runStateT stf s
              ma = runStateT sta s
              t (fab, _) = \(a, s) -> (fab a, s)
          in (t <$> mf) <*> ma
    in StateT { runStateT = newStateFunc }

instance Monad m => Monad (StateT s m) where
  return = pure
  sta >>= f =
    -- sequencing of actions
    let newStateFunc s =
          -- since the type of `runStateT sta` is `s -> m (a, s)`
          -- I can write (a, s) <- ma
          let ma = runStateT sta s -- action 1
          in do
              (a, s') <- ma
              let stb = f a -- intermediate value
              runStateT stb s'   -- action 2
    in StateT { runStateT = newStateFunc }
