#!/usr/bin/env stack runghc

-- real world haskell P/191
-- Either is a predefined type; use it for computation that 
-- could fail
-- Maybe gives us no information if a failure occurs: we 
-- literally have Nothing
-- The Either type has a similar structure, but instead of 
-- Nothing, the "something bad happened" ctor is named Left,
-- and it takes a parameter
-- often the type used for the a parameter is String, so we 
-- can provide a useful description if something goes wrong

canFail :: Int -> Either String Int
canFail n
        | n < 0     = Left "aa"
        | otherwise = Right n

main :: IO ()
main = do
  print $ canFail (-99)
  print $ canFail 878
