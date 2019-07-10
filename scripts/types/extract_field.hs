#!/usr/bin/env stack runghc

import Data.Typeable

-- real world haskell P/262
-- write a function that returns one of its arguments
-- some v _ _ _ _ = v
-- some function returns arg1

-- infoP is a function
-- \ (arg1, arg2) -> ret
type InfoP a =  Integer -- arg1
             -> Maybe Integer -- arg2
             -> a   -- ret

-- sizeP is really a value ctor!!
-- MY NOTE:
-- observe the type of sizeP 1123 (Just 1000), which is Integer
-- the value of sizeP 1123 (Just 1000) is 1000 which is of Integer
-- type; 
-- this pattern is how to write an extraction function in haskell
-- see (but may not be very helpful)
-- https://stackoverflow.com/questions/6047522/haskell-type-and-pattern-matching-question-extracting-fields-from-a-data-type
sizeP :: (InfoP Integer)
sizeP _ (Just sz) = sz
sizeP _ Nothing = -1

firstP :: (InfoP Integer)
firstP a _ = a

main :: IO ()
main = do
  print $ sizeP 1123 (Just 1000)
  print $ sizeP 1123 Nothing
  print $ firstP 213123 Nothing