#!/usr/bin/env stack runghc

{-# LANGUAGE TypeSynonymInstances #-}
-- real world haskell P/194
-- these two lang extensions are frequently used together

-- -XOverlappingInstances is deprecated: instead use per-instance 
-- pragmas OVERLAPPING/OVERLAPPABLE/OVERLAPS
-- https://downloads.haskell.org/~ghc/7.10.1/docs/html/users_guide/pragmas.html

import Data.List

class Foo a where
  foo :: a -> String

instance Foo a => Foo [a] where {
  foo = concat . intersperse ", " . map foo
}

instance Foo Char where
  foo c = [c]

-- instance Foo String where
--   foo = id

main :: IO ()
main = do
  print $ show ['a', 'b']

