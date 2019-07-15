#!/usr/bin/env stack runghc

import qualified Control.Monad.Reader as R
import qualified System.IO as IO

-- motivation: 
-- apply the knowledge of reader/writer monad to do something
-- useful
-- I want to do something similar to creating a Python class that 
-- wraps a file handle and then writes to it

foo :: [String] -> R.ReaderT IO.Handle IO () 
foo [] = return ()
foo (x:xs) = do
  h <- R.ask
  R.lift $ IO.hPutStrLn h x
  foo xs

main :: IO ()
main = do
  IO.withFile "/var/tmp/sut/tt.txt" IO.WriteMode $ \h -> do
    R.runReaderT (foo ["there", "is", "a", "cow"]) h
