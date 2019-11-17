module MoveCursor.ImplV4
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
          | West { dist :: Int }
          | East { dist :: Int }
          deriving (Show)
moveCursor :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor ms = do
  initialCursor <- R.ask
  R.lift $ W.tell initialCursor
  moveCursor' $ filter ((< 100) . dist) ms
moveCursor' :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor' []       = return ()
moveCursor' (m : ms) = do
  R.lift $ W.lift $ print m
  R.lift $ (W.tell . toCursor) m
  moveCursor' ms
toCursor :: Move -> Cursor
toCursor (North v) = Cursor 0 (-v)
toCursor (South v) = Cursor 0 v
toCursor (West  v) = Cursor (-v) 0
toCursor (East  v) = Cursor v 0
instance Semigroup Cursor where
  (<>) = mappend
instance Monoid Cursor where
  mempty = Cursor 0 0
  mappend (Cursor x1 y1) (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)
