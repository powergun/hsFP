module MoveCursor.ImplV3
  ( Cursor(..)
  , Move(..)
  , moveCursor
  )
where

import qualified Control.Monad.Reader          as R
import qualified Control.Monad.Writer          as W

data Cursor = Cursor Int Int deriving (Eq, Show)
data Move = North { dist :: Int }
          | South { dist :: Int }
          | East { dist :: Int }
          | West { dist :: Int }
          deriving (Show)

-- the purpose of `R.ReaderT Cursor` is to provide an initial 
-- state (Cursor) so that WriterT can modify this initial state
-- instead of the `empty` state (scratch value)
moveCursor :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor ms = do
  c <- R.ask
  R.lift $ W.tell c
  moveCursor' $ filter ((< 100) . dist) ms
moveCursor' :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor' []       = return ()
moveCursor' (m : ms) = do
  R.lift $ moveCursorW m
  moveCursor' ms

moveCursorW :: Move -> W.WriterT Cursor IO ()
moveCursorW m = do
  W.lift $ print m
  W.tell $ newCursor m

newCursor :: Move -> Cursor
newCursor (North v) = Cursor 0 (-v)
newCursor (South v) = Cursor 0 v
newCursor (West  v) = Cursor (-v) 0
newCursor (East  v) = Cursor v 0

instance Semigroup Cursor where
  (<>) = mappend

instance Monoid Cursor where
  mempty = Cursor 0 0
  (Cursor x1 y1) `mappend` (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)
