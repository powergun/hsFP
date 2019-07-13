#!/usr/bin/env stack runghc

import StateMonadTransformerV1

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

example :: Int -> StateT Int IO ()
example j = do
  i <- get
  lift $ putStrLn $ "current state is " ++ (show i)
  put j
  i' <- get
  lift $ putStrLn $ "current state is " ++ (show i')

main :: IO ()
main = do
  (_, state) <- runStateT (example 10) 100
  assert $ 10 == state
  (_, state') <- runStateT (example 1234) 12
  assert $ 1234 == state'
