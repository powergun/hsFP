module IOAndSharing (demo) where

import           Control.Concurrent
import           Debug.Trace

myData :: IO (MVar Int)
myData = newEmptyMVar

-- First Principles P/1162
-- In other words, the two references to myData here are not referring
--  to the same MVar
-- Taking from an empty MVar blocks until something is put into the MVar.
demoMVar :: IO ()
demoMVar = do
  print "//// demo IO MVar"
  mv <- myData
  putMVar mv 0
  mv' <- myData

  -- runtime error: deadlock
  -- zero <- takeMVar mv'

  zero <- takeMVar mv
  print zero

blah :: IO String
blah = return "blah"
-- outter trace
blah' = trace "outer trace" blah

-- inner trace
woot :: IO String
woot = return (trace "inner trace" "woot")

demoTracingIO :: IO ()
demoTracingIO = do
  print "//// demo tracing"
  b <- blah'
  putStrLn b
  putStrLn b
  w <- woot
  putStrLn w
  putStrLn w

demo :: IO ()
demo = do
  demoMVar
  demoTracingIO
