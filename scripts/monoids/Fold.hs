#!/usr/bin/env stack runghc

-- programming haskell L5682
-- to combine all the values in a data structure to give a
-- single value.
fold_ :: Monoid a => [a] -> a
fold_ []       = mempty
fold_ (x : xs) = x `mappend` fold_ xs

main :: IO ()
main =
  print $ fold_ ["asd", "iddqd"]
