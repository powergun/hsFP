#!/usr/bin/env stack runghc

-- |function type
-- |:: is called hastype
-- |() a unit: meaning main produces no value

import Data.Complex
import Data.Ratio

-- real world haskell P/77
-- Type variables always start with a lowercase letter. You can
--  always tell a type variable from a normal variable by context, 
-- because the languages of types and functions are separate: type 
-- variables live in type signatures, and regular variables live 
-- in normal expressions.

-- numeric types

n0 :: Int
n0 = 5
n1 :: Double
n1 = 5.0
n2 :: Complex Double
n2 = 2 :+ 3
n3 :: Ratio Int
n3 = 2 % 3

-- character types

c0 :: Char
c0 = 'X'
c1 :: Char
c1 = '\0088'
c2 :: Char
c2 = '\x0058'
c3 :: Char
c3 = '\o0130'

-- string types

s0 :: String
s0 = "abc"

s1 :: String
s1 = "\0088\x0058\o0130"

main :: IO ()
main = do
    print n0
    print n1
    print n2
    print n3
    print c0
    print c1
    print c2
    print c3
    print s0
    print s1
