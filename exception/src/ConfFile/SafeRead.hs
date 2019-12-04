module ConfFile.SafeRead (demo) where

import           Data.Maybe (listToMaybe)

-- listToMaybe
-- listToMaybe :: [a] -> Maybe aSource#

-- The listToMaybe function returns Nothing on an empty list or Just a where a is the first element of the list.

-- worth checking other useful functions in Data.Maybe

demo :: IO ()
demo = do
  let safeParseInt :: String -> Maybe Int
      safeParseInt = fmap fst . listToMaybe . reads

  print $ safeParseInt "2112" -- success
  print $ safeParseInt "0x1123" -- success
  print $ safeParseInt "213a1" -- only 213 is parsed!
  print $ safeParseInt "a213" -- fail (Nothing)

