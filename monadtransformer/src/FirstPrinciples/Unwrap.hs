module FirstPrinciples.Unwrap
  ( demo
  )
where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

-- MaybeT is the outter monad (the base monad)
embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

-- after unwrapping, Maybe (MaybeT) becomes the inner value
maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

-- ditto
eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

-- ditto
readerUnwrap :: () -> IO (Either String (Maybe Int))
readerUnwrap = runReaderT eitherUnwrap

{-
MaybeT (ExceptT String (ReaderT () IO)) Int
MaybeT (ExceptT String (ReaderT () (Either a0))) Int

Expected type: () -> () -> IO (Either String (Maybe Int))
  Actual type: () -> () -> Either String (Maybe Int)
-}
embedded' :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded' = MaybeT . ExceptT . ReaderT $ return . (const . Right . Just $ 1)

-- the following assertion is to prove that, to get IO (Either String (Maybe Int))
-- I need to inject it to the IO monad via return
assert' = (const . Right . Just $ 1) :: () -> Either String (Maybe Int)

demo :: IO ()
demo = do
  print "//// Lexical inner structure, unwrap exercise"
  readerUnwrap () >>= print
  let f :: () -> IO (Either String (Maybe Int))
      f = return . (const . Right . Just $ 1)
  f () >>= print

