module MoveCursor.ImplV1
  ( Cursor(..)
  , Move(..)
  , applyMoves
  )
where

import qualified Control.Monad.State as S

data Cursor = Cursor Int Int deriving (Eq, Show)

data Move = North Int
          | South Int
          | West Int
          | East Int
          deriving Show

apply :: Cursor -> Move -> Cursor
apply (Cursor x y) (North i) = Cursor x (y - i)
apply (Cursor x y) (South i) = Cursor x (y + i)
apply (Cursor x y) (West  i) = Cursor (x - i) y
apply (Cursor x y) (East  i) = Cursor (x + i) y

-- L3235
-- supposes we have many moves that need to be applied to the
-- cursor position. Use state monad, in which state the current
-- position of the cursor is
-- whenever we wish to apply a move to the cursor, we will get
-- the current cursor in the current state, apply the move
-- and then put the changed cursor back into the state
-- MY NOTES:
-- I have been looking for this pattern
applyMoves :: [Move] -> S.State Cursor ()
applyMoves []       = return ()
applyMoves (x : xs) = do
  cursor <- S.get
  S.put (apply cursor x)
  applyMoves xs
