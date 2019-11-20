
module MaybeTrans.ImplV1 (MaybeT(..) )where

import qualified Control.Monad.Trans as Mt
import qualified Control.Monad as M

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
-- everything inside the do block executed in the underlying Monad m
-- whatever that is
x `bindMT` f = MaybeT $ do
                 -- Maybe a
                 unwrapped <- runMaybeT x
                 case unwrapped of
                    Nothing -> return Nothing
                    Just y -> runMaybeT (f y)

returnMT :: (Monad m) => a -> MaybeT m a
returnMT x = MaybeT $ return (Just x)

failMT :: (Monad m) => t -> MaybeT m a
failMT _ = MaybeT $ return Nothing

-- realworld haskell chapter 18 web version:
-- Because of the later Monad-is-a-subclass-of-Applicative-is-a-subclass-of-Functor 
-- change, I had to add these instances to make it compile:
instance (Functor f) => Functor (MaybeT f) where
  fmap f x = MaybeT $ fmap f <$> runMaybeT x

instance (Functor m, Monad m) => Applicative (MaybeT m) where
  pure x = MaybeT $ pure (Just x)
  f <*> x = MaybeT $ do
              f' <- runMaybeT f
              x' <- runMaybeT x
              case (f', x') of
                (Just f'', Just x'') -> return (Just (f'' x''))
                _ -> return Nothing

-- The thing that we intend to make a Monad instance is the partial
-- type MaybeT m: this has the usual single type parameter, a, 
-- that satisfies the requirements of the Monad typeclass.
instance (Monad m) => Monad (MaybeT m) where
  (>>=) = bindMT
  return = returnMT
  fail = failMT

-- to turn our type into a monad transformer, we must provide
-- an instance of the MonadTrans class, so that a user can access
-- the underlying monad
-- the underlying monad starts out with a type parameter of a:
-- we inject the Just constructor so it will acquire the type 
-- that we need, Maybe a
-- we then hide the monad with our MaybeT constructor.
instance Mt.MonadTrans MaybeT where
  lift m = MaybeT (Just `M.liftM` m)
