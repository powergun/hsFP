#!/usr/bin/env stack runghc

{-# LANGUAGE BangPatterns #-}

-- what is bang pattern
-- https://stackoverflow.com/questions/993112/what-does-the-exclamation-mark-mean-in-a-haskell-declaration
-- https://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form/6889335#6889335
-- (saved in evernote haskell_programming notebook)

-- https://downloads.haskell.org/~ghc/7.8.4/docs/html/users_guide/bang-patterns.html

f1 :: Int -> Int -> Int
f1 x  y  = x * y
f2 :: Int -> Int -> Int
f2 !x y = x * y
