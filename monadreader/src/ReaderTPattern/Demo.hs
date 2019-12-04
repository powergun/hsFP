module ReaderTPattern.Demo
  ( unsafeGetConfigFromFile
  , getConfigFromFile
  ) where

import           Control.Exception (SomeException, try)

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

data NoParseError = NoParseError String SomeException
-- use reads instead of read!!
-- see: https://stackoverflow.com/questions/5121371/how-to-catch-a-no-parse-exception-from-the-read-function-in-haskell
readString :: String -> Either NoParseError String
readString = undefined
readInt :: String -> Either NoParseError Int
readInt = undefined

getConfigFromFile :: FilePath -> IO (Either SomeException AppConfig)
getConfigFromFile = try . unsafeGetConfigFromFile
