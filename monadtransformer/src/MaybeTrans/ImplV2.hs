module MaybeTrans.ImplV2
    ( MaybeT(..)
    ,
    )
    where

import           Control.Applicative ((<|>))
import qualified Control.Applicative as Ca
import qualified Control.Monad       as M
import qualified Control.Monad.Trans as Mt

data MaybeT m a = MaybeT {
    runMaybeT :: m (Maybe a)
}

instance Monad m => Functor (MaybeT m) where
    fmap = M.liftM
instance Monad m => Applicative (MaybeT m) where
    pure = return
    (<*>) = M.ap
instance Monad m => Monad (MaybeT m) where
    (>>=) x f = MaybeT $ do
       unwrapped <- runMaybeT x
       case unwrapped of
            Nothing -> return Nothing -- m (Maybe ..)
            Just y  -> runMaybeT (f y) -- m (Maybe ..)
    return = MaybeT . return . Just
    fail _ = MaybeT $ return Nothing
instance Mt.MonadTrans MaybeT where
    lift m = MaybeT (Just `M.liftM` m)
instance Monad m => Alternative (MaybeT m) where
    empty = MaybeT $ return Nothing
    x <|> y = do
        unwrappedLeft <- runMaybeT x
        case unwrappedLeft of
            Nothing -> runMaybeT y
            Just something - return something
-- instance Monad m => M.MonadPlus (MaybeT m) where
--     mzero = Ca.empty
--     mplus = (<|>)

