module SimpleTransformer.ImplV2
  ( StateT(..)
  , get
  , put
  , lift
  )
where

newtype StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}

instance Functor m => Functor (StateT s m) where
  fmap f (StateT stateFunc) =
    let newStateFunc s = (\(xa, xs) -> (f xa, xs)) <$> (stateFunc s)
    in  StateT newStateFunc

instance Applicative m => Applicative (StateT s m) where
  pure x = StateT (\s -> pure (x, s))

  stf <*> sta =
    let newStateFunc s =
            let sf = runStateT stf s
                sa = runStateT sta s
                -- extract the function encapsulated in the embedded
                -- monad and use it to produce m b, which must also
                -- be embedded 
                func (f_a_b, _) = \(xa, xs) -> (f_a_b xa, xs)
            in  (func <$> sf) <*> sa
    in  StateT newStateFunc

instance Monad m => Monad (StateT s m) where
  return = pure

  sta >>= stfab =
    let newStateFunc s =
            let sa = runStateT sta s
            in  do
                  (a, s1) <- sa
                  runStateT (stfab a) s1
    in  StateT newStateFunc

get :: Monad m => StateT s m s
get = let newStateFunc s = pure (s, s) in StateT newStateFunc

put :: Monad m => s -> StateT s m ()
put s = let newStateFunc _ = pure ((), s) in StateT newStateFunc

lift :: Monad m => m a -> StateT s m a
lift ma =
  let newStateFunc s = do
        a <- ma
        return (a, s)
  in  StateT newStateFunc
