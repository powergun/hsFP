#!/usr/bin/env stack runghc

f0 :: String -> Int
f0 = length

f1 :: String -> (String, Int)
f1 x = (x, length x)

f2 :: [String] -> [(String, Int)]
f2 = map f1

demoCallFunctions :: IO ()
demoCallFunctions = do
    print $ f0 "there"
    print $ f1 "there is"
    print $ f2 ["there", "is", "a cow"]

main :: IO ()
main = do
  demoCallFunctions
