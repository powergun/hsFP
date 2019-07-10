#!/usr/bin/env stack runghc

-- real world haskell P/116
-- if a function ctor takes two or more arguments, we have the 
-- option of using it in infix form, where we place it between
-- its first and second arguments

joinTwo :: String -> String -> String
x `joinTwo` y = x ++ y

-- read world haskell P/165
-- for custom "operator-like" function, I can use infix style
-- pattern matching without back-ticks
-- however for the above name-based function, back-ticks are a 
-- must!! (at both the definition and the callsite)
(<<>>) :: String -> String -> String
x <<>> y = x ++ y

main :: IO ()
main = do
  print $ "asd" `joinTwo` "doom"
  print $ "asd" <<>> "doom"
