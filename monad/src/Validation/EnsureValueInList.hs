module Validation.EnsureValueInList (demo) where

import           Control.Monad (forM)
import           Data.List     (elem)

-- TODO: find out the monadic solution to replace the case statement

demo :: IO ()
demo = do
  let sourceOfTruth = [1..10]
      subject = [4..12]
      missing :: [String]
      missing = foldl hasValue [] subject
      hasValue mis x =
        case elem x sourceOfTruth of
          True  -> mis
          False -> mis ++ ["missing " ++ show x]
      missing' :: [String]
      missing' = foldr hasValue' [] subject
      hasValue' x mis =
        case elem x sourceOfTruth of
          True  -> mis
          False -> ["missing " ++ show x] ++ mis
  print missing
  print missing'
