#!/usr/bin/env stack runghc
-- haskell cookbook L2412

-- TODO: implement this example

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show)

instance Functor Tree where
  fmap f (Leaf x) = Leaf (f x)
  fmap f (Node l x r) = 
    Node (fmap f l) (f x) (fmap f r)

main :: IO ()
main = do
  print 1