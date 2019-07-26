#!/usr/bin/env stack runghc

-- haskell cookbook L3542

import qualified Control.Monad.Reader as R
import qualified Control.Monad.Writer as W
import ReadWriteMonadV2

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

demoMoves :: IO ()
demoMoves = do
  let ms = [ South 10
           , South 110
           , East 30
           , East 150
           , North 20
           , West 123 ]
  -- L3604
  -- we need to run the outer most monad (Reader Monad) first
  -- and then run the inner monad (Writer Monad)
  (_, c1) <- W.runWriterT (R.runReaderT (moveCursor ms) (Cursor 10 10)) 
  -- assert $ Cursor 10 10 == c1
  print c1

main :: IO ()
main = do
  demoMoves
