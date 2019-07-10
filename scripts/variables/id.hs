#!/usr/bin/env stack runghc

main :: IO ()
main = do
  print $ [1, 2, 3]
  -- id: return its input list unmodified
  print $ id [1, 2, 3]
