#!/usr/bin/env stack runghc

-- P/100
-- haskell provides a standard function, error :: String -> a
-- that we can call when something has gone terribly wrong 
-- (need a string "message" parameter)

-- it has a result type a, so that we can call it anywhere and 
-- it will always have the right type
-- however it does not return a value like normal function
-- instead it immediately aborts evaluation (and prints the msg)

-- P/101
-- a more controllable approach:
-- we can use Maybe type to represent the possibility of an error
-- if we want to indicate that an operation has failed, we can 
-- use the Nothing ctor. Otherwise we wrap the value with Just
-- NOTE: this is similar to Rust's Option: some(v), error("msg")

main :: IO ()
main = do
  error "wrong!"
  print 1



