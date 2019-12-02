module BestPractices.Readlines (readLine1, readLine2) where

import           BestPractices.ReadException
import           Control.Monad               (join)
import           Control.Monad.Catch         (MonadThrow)
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Data.Typeable               (Typeable)

readLine1 :: (MonadIO m, MonadThrow n, Read a, Typeable a) => m (n a)
readLine1 = fmap readM (liftIO getLine)

-- Without the usage of liftIO here, we'd need both MonadIO and
-- MonadThrow constraints.
readLine2 :: (MonadIO m, Read a, Typeable a) => m a
readLine2 = liftIO (join readLine1)
