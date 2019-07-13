#!/usr/bin/env stack runghc

-- haskell cookbook L3221
-- 

import CursorMonadV1
import qualified Control.Monad.State as S

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

demoMoves :: IO ()
demoMoves = do
  let moves = [ South 100
              , East 100
              , North 100
              , West 100 ]
      c1 = S.execState (applyMoves moves) (Cursor 0 0)
      c2 = S.execState (applyMoves moves) (Cursor 12 12)
  assert $ Cursor 0 0 == c1
  assert $ Cursor 12 12 == c2

main :: IO ()
main = do
  demoMoves
