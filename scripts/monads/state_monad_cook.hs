#!/usr/bin/env stack runghc

import StateMonadV3

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

testStateMonadCreation :: IO ()
testStateMonadCreation = do
  assert $ (1, 1) == (runState (State (\s -> (1, 1))) 0)

testStateAsFunctor :: IO ()
testStateAsFunctor = do
  let sa = State (\s -> (1, s))
      f n = n * 10 + 1
      sb = fmap f sa
  assert $ (11, 0) == (runState sb 0)

testStateAsApplicative :: IO ()
testStateAsApplicative = do
  let sa = (pure 1) :: State Int Int
      f n = n * 100 + n * 10 + 1
      sf = State (\s -> (f, s))
      sb = sf <*> sa
  assert $ (111, 0) == (runState sb 0)

testStateMonad :: IO ()
testStateMonad = do
  let sa = (pure 1) :: State Int Int
      -- a pure function
      f x = x * 1000 + x * 100 + x * 10 + 1
      -- wrapping the pure function in the Monad context
      g x = return ((f x) * (-1))
      sb = do
        n <- sa
        -- use return() to work with pure function
        -- see state_monad_diagram.png to understand this concept
        return (f n)
      sc = do
        n <- sa
        -- not having to call return() explicitly but g is not
        -- a pure function either
        g n
  assert $ (1111, 0) == (runState sb 0)
  assert $ (-1111, 0) == (runState sc 0)

testGetState :: IO ()
testGetState = do
  let sa = return 100
      sb = do
        x <- sa
        s <- get
        -- verify that I can make use of the state (-100)
        return (x + s)
  assert $ (0, -100) == (runState sb (-100))

testPutState :: IO ()
testPutState = do
  let sa = return 1
      sb = do
        x <- sa
        s <- get
        put (x * 100 + s)
        -- not computing nothing, return the input (which is 1 
        -- defined in "sa = return 1") as it is
        return x
  assert $ (1, 101) == (runState sb (1))

main :: IO ()
main = do
  testStateMonadCreation
  testStateAsFunctor
  testStateAsApplicative
  testStateMonad
  testGetState
  testPutState
