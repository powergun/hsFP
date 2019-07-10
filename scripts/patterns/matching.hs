#!/usr/bin/env stack runghc

-- real world haskell P/91

-- order is important
-- as we have already mentioned, a haskell implementation checks 
-- patterns for matches in the order in which we specify them in 
-- out equations
-- matching proceeds from top to bottom and stops at the first 
-- success.
-- Equations that are below a successful match has no effect

-- P/91
-- construction and deconstruction
-- first we check to see if the value was created using that 
-- ctor; if it was we inspect it to obtain the individual values
-- that we originally supplied to the ctor 
-- it let us look inside it

-- P/92
-- there is no limit on how "deep" within a value a pattern can
-- look.

-- P/93
-- we can pattern match algebraic type using its value ctor
-- we can extract the values 
-- the compiler can infer the types of the accessor functions 
-- based on the (value) ctor that we are using in our pattern
data MapInfo = MapInfo Int String
             | MapDesc String String
mapId (MapInfo id name) = id
mapName (MapInfo id name) = name
mapTitle (MapDesc code title) = title
-- P/94 suggesting wildcard to avoid unused variable name, 
-- mapTitle (MapDesc _ title) = title
demoExtractComponents :: IO ()
demoExtractComponents = do
  let mi = MapInfo 13 "e1m1"
      md = MapDesc "/e1/m1" "at hell's gate"
  print $ mapId mi
  print $ mapName mi
  print $ mapTitle md

main :: IO ()
main = do
  demoExtractComponents

