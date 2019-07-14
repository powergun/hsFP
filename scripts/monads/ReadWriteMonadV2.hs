module ReadWriteMonadV2 
  ( Cursor(..)
  , Move(..)
  , moveCursor
  ) where

import qualified Data.Monoid as M
import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W

data Cursor = Cursor Int Int deriving (Eq, Show)
data Move = North Int
          | South Int
          | West Int
          | East Int
          deriving (Eq, Show)

instance Semigroup Cursor where
  (Cursor x1 y1) <> (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)

instance M.Monoid Cursor where
  mempty = Cursor 0 0
  mappend = (<>)

moveCursor :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor ms = do
  -- in reader monad
  c <- R.ask
  R.lift (W.tell c)
  R.lift (moveCursor' ms)

  where
    moveCursor' [] = return ()
    moveCursor' (m:ms) = do
      W.lift (print $ "applying move " ++ (show m))
      updateCursor m
      moveCursor' ms

    updateCursor :: Move -> W.WriterT Cursor IO ()
    updateCursor = W.tell . toCursor

    toCursor :: Move -> Cursor
    toCursor (North n) = Cursor 0 (-n)
    toCursor (South n) = Cursor 0 n
    toCursor (West n) = Cursor (-n) 0
    toCursor (East n) = Cursor n 0 
