{-# LANGUAGE OverloadedStrings #-}

module HttpQuery
  ( demo
  )
where

import           Data.ByteString.Lazy    hiding ( map )
import           Network.Wreq
import           Control.Lens

demo :: IO ()
demo = do
  let urls :: [String]
      urls = ["http://httpbin.org"]
      mappingGet :: [IO (Response ByteString)]
      mappingGet = map get urls
      traversedUrls :: IO [Response ByteString]
      traversedUrls = traverse get urls
      r             = (fmap . fmap . fmap) (\s -> "asd") traversedUrls
  r >>= print
