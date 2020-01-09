{-# LANGUAGE BangPatterns #-}

module WithBinaryFileDemo (demo) where

import           Control.Monad.IO.Unlift
import           System.IO

-- source: https://github.com/fpco/unliftio#readme
-- beware of hGetContent: illegal operation (delayed read on closed handle)
-- see:
-- https://stackoverflow.com/questions/43910682/haskell-hgetcontents-error
-- https://stackoverflow.com/questions/26949378/what-caused-this-delayed-read-on-closed-handle-error

myWithBinaryFile
    :: MonadUnliftIO m
    => FilePath
    -> IOMode
    -> (Handle -> m a)
    -> m a
myWithBinaryFile fp mode inner =
  withRunInIO $ \runInIO ->
  withBinaryFile
    fp
    mode
    (\h -> runInIO (inner h))

demo :: IO ()
demo = do
  let func :: Handle -> IO (Maybe Char)
      func fh = do
        c <- hGetChar fh
        return . Just $ c
  myWithBinaryFile "testdata/test.bin" ReadMode func >>= print
