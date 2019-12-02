module BestPractices.ReadException
  ( ReadException(..)
  , readM
  , SomeException(..)
  ) where

-- source:
-- https://www.fpcomplete.com/blog/2016/11/exceptions-best-practices-haskell

import           Control.Exception   (Exception, SomeException)
import           Control.Monad.Catch (MonadThrow, throwM)
import           Data.Typeable       (TypeRep, Typeable, typeRep)
import           Text.Read           (readMaybe)

data ReadException = ReadException String TypeRep
  deriving (Typeable)

instance Show ReadException where
  show (ReadException s typ) = concat
    [ "Unable to parse as "
    , show typ
    , ": "
    , show s
    ]

instance Exception ReadException

readM :: (MonadThrow m, Read a, Typeable a) => String -> m a
readM s = res
  where
    res =
      case readMaybe s of
        Just x  -> return x
        Nothing -> throwM $ ReadException s (typeRep res)
