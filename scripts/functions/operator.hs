#!/usr/bin/env stack runghc
-- see how to design and define custom operators
-- binparse / PNM
-- binparse / Parse 

-- 
(...) :: (b -> c) -> (a -> b) -> a -> c
(...) f1 f2 =
  -- use pointed form
  -- f1 (f2 a) 

  -- use pointless form 
  f1 . f2
demoCustomDot :: IO ()
demoCustomDot = do
  print "//// demo custom dot operator"
  print $ ((\x -> x + 1) ... (\x -> x * 3)) 2
  print $ ((\xs -> map ((*) 3) xs) ... (\xs -> map ((+) 2) xs)) [1, 2, 3]

-- real world haskell P/284
-- NOTE: use this simplified demo to understand the book example
-- see binparse / Parse


main :: IO ()
main = do
  demoCustomDot
