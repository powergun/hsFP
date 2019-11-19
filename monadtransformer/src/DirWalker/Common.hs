module DirWalker.Common (listDirectory) where

import           Control.Monad    (liftM)
import           System.Directory (getDirectoryContents)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where
        notDots p = p /= "." && p /= ".."
