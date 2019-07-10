#!/usr/bin/env stack runghc

-- inspired by udemy mastering haskell programming
-- what is side effect? what is unsafe IO?

-- NOTE how do {} block forces the function sig to be IO Int
foo :: Int -> IO Int
foo x = do
  print "asd"
  return x

main :: IO ()
main = do
  -- the caller of the IO function must also follow the monad
  -- idiom (in this case, the do block)
  x <- foo 12
  print x
