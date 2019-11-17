module TestMoveCursor
  ( runSpec
  )
where

import qualified Control.Monad.Reader          as R
import qualified Control.Monad.Writer          as W
import           MoveCursor.ImplV4

assert :: Bool -> IO ()
assert True  = return ()
assert False = error "fail"

demoMoves :: IO ()
demoMoves = do
  let ms = [South 10, South 110, East 30, East 150, North 20, West 123]
  -- L3604
  -- we need to run the outer most monad (Reader Monad) first
  -- and then run the inner monad (Writer Monad)
  let mw = R.runReaderT (moveCursor ms) (Cursor 10 10)
  (_, c1) <- W.runWriterT mw
  assert $ Cursor 40 0 == c1
  print c1

runSpec :: IO ()
runSpec = demoMoves

