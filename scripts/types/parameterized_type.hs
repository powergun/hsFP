#!/usr/bin/env stack runghc

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- NOTE:
-- the follow examples may slightly overlap with types/methods
-- but these examples are taken from a more focused explanation
-- from book Haskell Cookbook L1849
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- real world haskell P/97
-- 
-- data Maybe a = Just a
--              | Nothing

-- a (righthand side) is not regular variable - it is a type variable
-- it indicates that the Maybe type takes another type as its
-- parameter

-- P/97 a shallow analogy: parameterized types bear some 
-- resemblance to C++ template and Java generics

-- P/99
-- (comparing to Java which has Null type) in haskell we don't
-- have an equivalent of null. We could use the Maybe type to 
-- provide a similar effect
-- or use a no-argument Empty value ctor

-- haskell cookbook L1835
newtype Func a b = Func (a -> b)
compose :: Func a b -> Func b c -> Func a c
compose (Func f) (Func g) = Func (g . f) -- note the order of .
apply :: Func a b -> a -> b
apply (Func f) x = f x -- note how it takes adv of data ctor

-- see:
-- https://en.wikibooks.org/wiki/Haskell/Fix_and_recursion
-- fix :: (a -> a) -> a
newtype Fix f = Fix (f (Fix f))

-- haskell cookbook P/1905
-- Ghost type is called Phantom type
-- very useful in programs as we can use them to add a type 
-- annotation to our definition
-- see
-- https://wiki.haskell.org/Phantom_type
data Ghost a = Ghost -- can NOT use newtype here (one and only param rule) 
               deriving (Show)

demoFuncCompose :: IO ()
demoFuncCompose = do
  let f1 = Func (\n -> n + 1) 
      f2 = Func (\n -> "result: " ++ show n)
  print $ apply (compose f1 f2) 1

demoFixGhost :: IO ()
demoFixGhost = do
  let x = Ghost 
      y = Fix x
      Fix z = y
  print x
  print z

main :: IO ()
main = do
  demoFuncCompose
  demoFixGhost

