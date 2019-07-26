#!/usr/bin/env stack runghc

import Data.Char

-- real world haskell P/254
-- forM is mapM with its arguments flipped

-- mapM :: (a -> m b) -> [a] -> m [b]
demoMapMWithIO :: IO ()
demoMapMWithIO = do
  print
    "//////// demo mapM with IO monad /////////////////////////"
  mapM process [1, 2, 3, 4] >>= print

  -- trash the result
  mapM_ process [1, 2, 3]
  where
    process elem = do
      print $ "processing: " ++ (show elem)
      let e = elem * 2 + 1
      return e

-- demonstrate that mapM works with any monad
-- programming haskell P173
-- applying mapM to the conv function gives a means of converting
-- a string of digits into the corresponding list of numeric
-- values, which succeeds if every character in the string is 
-- a digit, and fails otherwise
-- MY NOTES:
-- recall mapM takes [a] and produces "m [b]"
demoMapM :: IO ()
demoMapM = do
  print
    "//////// demo mapM ///////////////////////////////////////"
  let conv :: Char -> Maybe Int
      conv c | isDigit c = Just (digitToInt c)
             | otherwise = Nothing
  print $ mapM conv "123"
  print $ mapM conv "123a"

main :: IO ()
main = do
  demoMapMWithIO
  demoMapM
