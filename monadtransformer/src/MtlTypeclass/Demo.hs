{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}

module MtlTypeclass.Demo (demo) where

-- source
-- https://tech.fpcomplete.com/haskell/tutorial/monad-transformers

import qualified Control.Monad.State as State

-- this means the type of monad m, determines the type of state, s
-- we use it so that type inference continues to work nicely
class Monad m => MonadState s m | m -> s where
  get :: m s
  put :: s -> m ()

-- define an instance for State itself
instance MonadState s (State.State s) where
  get = State.get
  put = State.put

demo :: IO ()
demo = return ()

