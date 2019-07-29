module StateMonadTransformerV1 
  ( StateT(..)
  , get
  , put
  , lift
  ) where

-- haskell cookbook L3364
-- embed another monad into a state monad, hence all actions are
-- performed in the embedded monad, whereas the state transformer
-- is responsible for keeping state
-- L3425
-- remember that all the actions are performed in the internal 
-- monad... we took advantage of the do... syntax for the monad
-- all the internal monadic actions are modified from m a to 
-- m (a, s)
-- all monad transformers follow a similar strategy

-- in the mtl implementation of state transformer you will see
-- two impls, lazy and strict. 
-- In the strict version, the state actions are sequenced using 
-- seq

-- note how we embed middle m (monad) in the type
newtype StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}

instance Functor m => Functor (StateT s m) where
  fmap f sta =
    let newStateFunc s = 
          let t (xa, xs) = (f xa, xs)
              ma = runStateT sta s
          in t <$> ma
    in StateT newStateFunc

-- we make use of the fact taht the embedded monad is also an 
-- insatnce of Applicative and use it to lift the embedded
-- Applicative instance, that is, m (a -> b) -> m a -> m b
-- to our state related 
-- m ( (a, s) -> (b, s) ) -> m (a, s) -> m (b, s)
instance Applicative m => Applicative (StateT s m) where
  -- L3383
  -- use the applicative instance of the embedded applicative
  -- to induce both x as well as state s into it

  -- pure x = 
  --   let stateFunc s = pure (x, s)
  --   in StateT stateFunc
  
  -- MY NOTES:
  -- rewrite to follow the State Monad (v3) pattern
  pure x = StateT (\s -> pure (x, s))

  -- L3383
  -- get a function from State s m (a -> b) and apply it to
  --                     ^^^^^^^^^^^^^^^^^
  -- State s m a to produce State s m b
  -- ^^^^^^^^^^^            ^^^^^^^^^^^
  stf <*> sta = 
    let newStateFunc s = 
          let mf = runStateT stf s
              ma = runStateT sta s
              -- t for transform
              t (fab, _) = \(a, s) -> (fab a, s)
          -- in the inner monad
          in (t <$> mf) <*> ma
    in StateT newStateFunc

instance Monad m => Monad (StateT s m) where
  return = pure

  -- st m a -> (a -> st m b) -> st m b
  sta >>= stf =
    let newStateFunc s =
          let ma = runStateT sta s -- m (a, s)
          in do
              (a, s1) <- ma
              let stb = stf a -- stf a produces st m b
                              -- this is the "sequencing" part in 
                              -- standard Monad bind operator
              runStateT stb s1 -- smb with state updated to s1
    in StateT newStateFunc

get :: Monad m => StateT s m s
get = 
  let newStateFunc s = pure (s, s)
  in StateT newStateFunc

put :: Monad m => s -> StateT s m ()
put s = 
  let newStateFunc _ = pure ((), s)
  in StateT newStateFunc

-- L3399 
-- we should allow an operation in the embedded monad in the 
-- context of our state transformer
lift :: Monad m => m a -> StateT s m a
lift ma =
  let newStateFunc s = do
        a <- ma
        return (a, s)
  in StateT newStateFunc