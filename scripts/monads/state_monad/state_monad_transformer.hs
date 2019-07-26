#!/usr/bin/env stack runghc

import StateMonadTransformerV2

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

-- embed IO monad in the state transformer
-- get the current state and modify it with the supplied argument
-- recall:
-- all actions are
-- performed in the embedded monad, whereas the state transformer
-- is responsible for keeping state
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
