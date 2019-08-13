#!/usr/bin/env stack runghc

import           Data.Maybe

main :: IO ()
main = do
  -- para con haskell L568
  -- use isJust to filter a list of Maybe values
  print . isJust $ Just 10
  print . isJust $ (Nothing :: Maybe Int)
