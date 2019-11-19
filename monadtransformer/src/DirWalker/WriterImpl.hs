module DirWalker.WriterImpl (countEntries) where

import           Control.Monad        (forM_, when)
import           Control.Monad.Trans  (liftIO)
import qualified Control.Monad.Writer as Mw
import           System.Directory     (doesDirectoryExist)
import           System.FilePath      ((</>))

import           DirWalker.Common

countEntriesImpl :: FilePath -> Mw.WriterT [(FilePath, Int)] IO ()
countEntriesImpl dirp = do
    contents <- liftIO . listDirectory $ dirp
    Mw.tell [(dirp, length contents)]
    forM_ contents processPath
    where
        processPath filename = do
            let fullPath = dirp </> filename
            isDir <- liftIO . doesDirectoryExist $ fullPath
            when isDir $ countEntriesImpl fullPath

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries dirp = do
    (_, recorded) <- Mw.runWriterT (countEntriesImpl dirp)
    return recorded
