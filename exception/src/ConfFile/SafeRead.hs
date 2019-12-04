module ConfFile.SafeRead (demo) where

import           Data.Maybe (listToMaybe)

demo :: IO ()
demo = do
  let safeParseInt :: String -> Maybe Int
      safeParseInt = fmap fst . listToMaybe . reads

  print $ safeParseInt "2112" -- success
  print $ safeParseInt "0x1123" -- success
  print $ safeParseInt "213a1" -- only 213 is parsed!
  print $ safeParseInt "a213" -- fail (Nothing)

