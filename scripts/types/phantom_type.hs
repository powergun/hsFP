#!/usr/bin/env stack runghc

-- mentioned in haskell cookbook L1905
-- full wiki:
-- https://wiki.haskell.org/Phantom_type

-- use this mechanism to implement an awkward dispatch routine

data Option = Quoted
            | Unquoted

toStr :: (Show a) => a -> Option -> String
toStr some Quoted = show some
toStr some Unquoted = init (tail (show some))

main :: IO ()
main = do
  print $ toStr [1] Quoted
  print $ toStr "asdasd" Unquoted
