module FirstPrinciples.JoinBindFmap (demo) where

-- source: some Sydney haskell school group chat

-- implement join and fmap in terms of bind
-- implement bind in terms of join and fmap

import qualified Control.Monad

-- f >>= ma = join $ fmap f ma
-- source:
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/src/GHC.Base.html#join
join' :: (Monad m) => m (m a) -> m a
join' mma = mma >>= id

fmap' :: (Monad m) => (a -> b) -> m a -> m b
fmap' f ma = ma >>= (return . f)

demo :: IO ()
demo = return ()
