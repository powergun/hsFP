module MoveCursor.ImplV2
  ( Cursor(..)
  , Move(..)
  , moveCursor
  )
where

import qualified Data.Monoid                   as M
import qualified Control.Monad.Reader          as R
import qualified Control.Monad.Writer          as W

data Cursor = Cursor Int Int deriving (Eq, Show)
data Move = North { dist :: Int }
          | South { dist :: Int }
          | West { dist :: Int }
          | East { dist :: Int }
          deriving (Eq, Show)

instance Semigroup Cursor where
  (Cursor x1 y1) <> (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)

instance M.Monoid Cursor where
  mempty  = Cursor 0 0
  mappend = (<>)

moveCursor :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor ms = do
  -- in reader monad
  c <- R.ask
  R.lift (W.tell c)

  -- apply some logic inside reader monad
  R.lift (moveCursor' $ filter ((< 100) . dist) ms)

 where
    -- in writer monad
  moveCursor' :: [Move] -> W.WriterT Cursor IO ()
  moveCursor' []       = return ()
  moveCursor' (m : ms) = do
    W.lift (print $ "applying move " ++ (show m))
    W.tell $ toCursor m
    moveCursor' ms

toCursor :: Move -> Cursor
toCursor (North n) = Cursor 0 (-n)
toCursor (South n) = Cursor 0 n
toCursor (West  n) = Cursor (-n) 0
toCursor (East  n) = Cursor n 0
