module FirstPrinciples.Unwrap
  ( demo
  )
where

import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = return 1

maybeUnwrap :: ExceptT String (ReaderT () IO) (Maybe Int)
maybeUnwrap = runMaybeT embedded

eitherUnwrap :: ReaderT () IO (Either String (Maybe Int))
eitherUnwrap = runExceptT maybeUnwrap

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

assert' :: () -> Either String (Maybe Int)
assert' = (const . Right . Just $ 1)

demo :: IO ()
demo = do
  print "//// Lexical inner structure, unwrap exercise"
  readerUnwrap () >>= print
  let f :: () -> IO (Either String (Maybe Int))
      f = return . (const . Right . Just $ 1)
  f () >>= print

