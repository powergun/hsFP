module DirWalker.TransStackImpl (countEntries) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import qualified Control.Monad.Trans as Mt
import qualified Control.Monad.Reader as Mr
import qualified Control.Monad.State as Ms

data AppConfig = AppConfig {
  cfgMaxDepth :: Int
} deriving (Show)

data AppState = AppState {
  stDeepestReached :: Int
} deriving (Show)

-- deliberately missing a the type parameter
type App = Mr.ReaderT AppConfig (Ms.StateT AppState IO)

-- a is the type of the "return type" - i.e. what the ReaderT
-- will return to the caller
runApp :: App a -> Int -> IO (a, AppState)
-- k is `App a`, which is produced by the function that carries 
-- the business logic (i.e. the traversal function)
-- inside this function it has all the benefits of config and 
-- state
runApp k maxDepth =
  let config = AppConfig maxDepth
      state = AppState 0
  in Ms.runStateT (Mr.runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath, Int)]
constrainedCount curDepth path = do
  contents <- Mt.liftIO . listDirectory $ path
  cfg <- Mr.ask
  result <- Mr.forM contents $ \name -> do
              let newPath = path </> name
              isDir <- Mt.liftIO $ doesDirectoryExist newPath
              if isDir && curDepth < cfgMaxDepth cfg
                then do
                  let newDepth = curDepth + 1
                  st <- Ms.get
                  Ms.when (stDeepestReached st < newDepth) $
                    Ms.put st { stDeepestReached = newDepth }
                  constrainedCount newDepth newPath
                else return []
  return $ (path, length contents) : concat result

countEntries :: Int -> FilePath -> IO [(FilePath, Int)]
countEntries maxDepth path = do
  let app = constrainedCount 0 path
  (result, state) <- runApp app maxDepth
  return result
