#!/usr/bin/env stack runghc

-- to demystify the meaning of instance Functor (State s t k)
-- s t k are type parameters

data State s t k a = State {
    runState :: s -> (a, s)
  }

instance Functor (State s t k) where -- type constructor
  fmap f (State stateFunc) =
    let newStateFunc s =
          let (x, s') = stateFunc s
          in (f x, s')
    in State { runState = newStateFunc }

testStateAsFunctor :: IO ()
testStateAsFunctor = do
  let sa = State (\s -> (1, s))
      f n = n * 10 + 1
      sb = fmap f sa
  print $ (11, 0) == (runState sb 0)

main :: IO ()
main = do
  testStateAsFunctor
