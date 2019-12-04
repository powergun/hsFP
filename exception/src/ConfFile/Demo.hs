{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module ConfFile.Demo
  ( unsafeGetConfigFromFile
  , getConfigFromFile
  , runProgram
  ) where

import           Control.Exception    (Exception, IOException, SomeException,
                                       try)
import           Control.Monad.Except (ExceptT, MonadError, MonadIO, liftIO,
                                       runExceptT, throwError)
import           Data.Bifunctor       (first)
import           Data.Bool            (bool)

data AppConfig = AppConfig { mapName :: String
                           , mapCode :: String
                           , mapSize :: Int
                           } deriving (Show)

unsafeGetConfigFromFile :: FilePath -> IO AppConfig
unsafeGetConfigFromFile filename = do
  s <- readFile filename
  return $ AppConfig
        <$> (\[x, _, _] -> x)
        <*> (\[_, x, _] -> x)
        <*> (\[_, _, x] -> (read x) :: Int)
        $ lines s

readFileSafe :: FilePath -> IO (Either IOException String)
readFileSafe = try . readFile

data ConfError = NoParseError String
               | FileError String IOException
               deriving (Show)

-- I have to introduce this (borrowed from hsSysAdmin/thecli project)
-- in order that I can call throwError in the parsing function and
-- getLines() function
newtype Conf a = Conf {
  runConf :: ExceptT ConfError IO a
} deriving ( Monad, Applicative, Functor
           , MonadError ConfError
           , MonadIO
           )

doConf :: FilePath -> Conf AppConfig
doConf fn = do
  -- experiment: this will appear at the test suite
  --  ```NoParseError "throwing a hardcoded error!" ```
  -- throwError $ NoParseError "throwing a hardcoded error!"
  let readFileSafe :: FilePath -> IO (Either ConfError String)
      readFileSafe filename =
        first (FileError "readFileSafe()") <$> (try . readFile $ filename)
      rf :: FilePath -> Conf String
      rf filename = do
            r <- liftIO $ readFileSafe filename
            either throwError return r
  t <- rf fn
  xs <- getLines t
  let [a, b, c] = xs
  c' <- readInt c
  return $ AppConfig a b c'

runProgram :: FilePath -> IO ()
runProgram fn =
  either print print =<< runExceptT (runConf $ doConf fn)

-- this has to run inside Conf monad in order to cover the exception:
-- where there are not exactly three lines in the config file
getLines :: String -> Conf [String]
getLines x =
  let rr :: Either ConfError [String]
      rr = case (lines x) of
              xs@[a, b, c] -> Right xs
              _            -> Left $ NoParseError $ "bad config content"
  in either throwError return rr

-- use reads instead of read!!
-- see: https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell

-- NOTE: this function is useless and is causing parsing failure!!

-- readString :: String -> Conf String
-- readString x =
--   let rr :: Either ConfError String
--       rr = case (reads x) :: [(String, String)] of
--               (v, _):_ -> Right v
--               _        -> Left $ NoParseError $ "bad str: " ++ x
--   in either throwError return rr

-- the parsing function must run in Conf monad to cover the parsing failure
readInt :: String -> Conf Int
readInt x =
  let rr :: Either ConfError Int
      rr = case (reads x) :: [(Int, String)] of
              (v, _):_ -> Right v
              _        -> Left $ NoParseError $ "bad int: " ++ x
  in either throwError return rr

getConfigFromFile :: FilePath -> IO (Either SomeException AppConfig)
getConfigFromFile = try . unsafeGetConfigFromFile
