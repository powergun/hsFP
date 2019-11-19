module DirWalker.BaseImpl (countEntriesTrad) where

import           Control.Monad    (forM)
import           DirWalker.Common
import           System.Directory (doesDirectoryExist)
import           System.FilePath  ((</>))

countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
    contents <- listDirectory path
    rest <- forM contents $ \name -> do
            let newName = path </> name
            isDir <- doesDirectoryExist newName
            if isDir
                then countEntriesTrad newName
                else return []
    return $ (path, length contents) : concat rest
