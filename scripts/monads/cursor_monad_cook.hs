#!/usr/bin/env stack runghc

-- haskell cookbook L3221
-- 

import Prelude hiding (Left, Right)
import qualified Control.Monad.State as S

data Cursor = Cursor Int Int deriving Show

data Move = Up Int
          | Down Int
          | Left Int
          | Right Int
          deriving Show

apply :: Cursor -> Move -> Cursor
apply (Cursor x y) (Up i) = Cursor x (y - i)
apply (Cursor x y) (Down i) = Cursor x (y + i)
apply (Cursor x y) (Left i) = Cursor (x - i) y
apply (Cursor x y) (Right i) = Cursor (x + i) y

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
applyMoves [] = return ()
applyMoves (x:xs) = do
  cursor <- S.get
  S.put (apply cursor x)
  applyMoves xs

demoMoves :: IO ()
demoMoves = do
  let moves = [ Down 100
              , Right 100
              , Up 100
              , Left 100 ]
  print $ S.execState (applyMoves moves) (Cursor 0 0)
  print $ S.execState (applyMoves moves) (Cursor 12 12)

main :: IO ()
main = do
  demoMoves
