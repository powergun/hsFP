module TwoFilesProblem (demo) where

import qualified Control.Exception as E
import Control.Monad.Except

demo :: (FilePath, FilePath) -> IO ()
demo (filename1, filename2) = 
  either renderError return =<< runExceptT runMain
  where
    runMain :: ExceptT E.IOException IO ()
    runMain = do
      -- can not compile if using readFile here 
      -- Couldn't match type ‘IO’ with ‘ExceptT E.IOException IO’
      -- this strongly informs the caller the potential of exception;
      -- and force the caller (of runMain) to handle the exception
      file1 <- readFileWithFailure filename1
      file2 <- readFileWithFailure filename2
      liftIO $ processResult file1 file2
    readFileWithFailure :: FilePath -> ExceptT E.IOException IO String
    readFileWithFailure s = 
      either throwError return =<< liftIO (safeReadFile s)

renderError :: Show e => e -> IO ()
renderError e = putStrLn $ "ERROR: " ++ show e

processResult :: String -> String -> IO ()
processResult s s' = 
  putStrLn $ "Result:\n------------\n" ++ s ++ "\n------------\n" ++ s'

safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile
