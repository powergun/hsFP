#!/usr/bin/env stack runghc

-- for a good example, see regex / Glob.hs 
-- (see the large function-guard statement)
-- taken from real world haskell P/247

-- programming haskell L1286
-- | is read as such that, and the guard otherwise is defined in the 
-- standard prelude simply by otherwise = True

abs_ n | n >= 0 = n
       | otherwise = -n

-- the main benefit of guarded equations over conditional expressions
-- is that definitions with multiple guards are easier to read

cap_ n | n > 123 = 123
       | n < 10 && n > -10 = 4
       | n < (-123) = (-123)
       | otherwise = n

-- real world haskell P/108
-- use pattern match to ensure that we are looking at values of 
-- the right shape (same type); use a guard to compare the pieces
tuplesAreSame :: (String, Int, Int) -> (String, Int, Int) -> Maybe Int
tuplesAreSame (a, _, _) (b, _, _)
  | a == b        = Just 0
  | a > b         = Just 1
  | a < b         = Just (-1)
tuplesAreSame _ _ = Nothing

demoTuplesAreSame :: IO ()
demoTuplesAreSame = do
  print $ tuplesAreSame ("iddqd", 0, 1) ("iddqd", 324234, 123)
  print $ tuplesAreSame ("idkfa", 0, 1) ("idfa", 0, 1)

main :: IO ()
main = do
    print $ map cap_ [-123123, 1, 3, 13434]
    demoTuplesAreSame
