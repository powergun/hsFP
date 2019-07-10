#!/usr/bin/env stack runghc

-- real world haskell P/144
-- what if we want a function that behaves like tails but only 
-- returns the non-empty suffixes

-- as-pattern @
-- bind the variable to the value that matches the right hand
-- side of the @ symbol

nonEmptyTails :: [a] -> [[a]]
-- if the pattern after the @ matches, xs will be bound to the 
-- entire list that matched, and xs' will be bound to all but 
-- the head of the list (don't care about the head element)
-- P/144 when we defined nonEmptyTails, we reused the the value
-- xs that we matched with our as-pattern
-- since we reused existing value, we avoid a little allocation
nonEmptyTails xs@(_:xs') = xs : nonEmptyTails xs' 
nonEmptyTails _ = []
demoNonEmptyTails :: IO ()
demoNonEmptyTails = do
  print "//// demo non empty tails (as pattern)"
  print $ nonEmptyTails "thereis"
  

main :: IO ()
main = do
  demoNonEmptyTails
