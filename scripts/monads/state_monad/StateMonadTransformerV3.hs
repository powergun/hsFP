module StateMonadTransformerV3
  ( StateT(..)
  , get
  , put
  , lift
  ) where

data StateT s m a = StateT { stateFunc :: s -> m (a, s) }

get :: Monad m => StateT s m a
