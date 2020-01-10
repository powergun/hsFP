{-# LANGUAGE TupleSections #-}

module DBQueryPipeline
  ( demo
  )
where

-- source: First Principles P/854

import           Control.Monad                  ( mapM )

data Query = Query
data SomeObj = SomeObj deriving (Show)
data IoOnlyObj = IoOnlyObj deriving (Show)
data Err = Err deriving (Show)

-- from IO [String] to IO (Either Err [SomeObj])
decodeFn :: String -> Either Err SomeObj
decodeFn _ = Right SomeObj

fetchFn :: Query -> IO [String]
fetchFn query = return ["iddqd"]

-- from `IO (Either Err [SomeObj])` to `IO (Either Err [(SomeObj, IoOnlyObj)])`
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj os = return $ fmap (, IoOnlyObj) os

pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn =
  -- do
  -- a <- fetchFn query
  -- traverse makeIoOnlyObj (mapM decodeFn a)
  (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn

demo :: IO ()
demo = do
  let q = Query
  print =<< pipelineFn q
