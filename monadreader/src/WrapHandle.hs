module WrapHandle
    ( demo
    )
where

import qualified Control.Monad.Reader          as Mr
import qualified System.IO                     as IO

-- motivation: 
-- apply the knowledge of reader/writer monad to do something useful
-- I want to do something similar to creating a Python class that 
-- wraps a file handle and then writes to it

writeToHandle :: [String] -> Mr.ReaderT IO.Handle IO ()
writeToHandle []       = return ()
writeToHandle (x : xs) = do
    handle <- Mr.ask
    Mr.lift $ IO.hPutStrLn handle x
    writeToHandle xs

readFromHandle :: Mr.ReaderT IO.Handle IO ()
readFromHandle = do
    handle <- Mr.ask
    Mr.lift $ do
        s <- IO.hGetContents handle
        putStrLn $ filter (/= '\n') s
        return ()
    return ()

-- IO.withFile
-- http://hackage.haskell.org/package/base-4.12.0.0/docs/System-IO.html
-- withFile name mode act
demo :: IO ()
demo = do
    let wact handle =
            Mr.runReaderT (writeToHandle ["there", "is", "a", "cow"]) handle
    let ract handle = Mr.runReaderT (readFromHandle) handle
    IO.withFile "/var/tmp/sut/tt.txt" IO.WriteMode wact
    IO.withFile "/var/tmp/sut/tt.txt" IO.ReadMode ract
