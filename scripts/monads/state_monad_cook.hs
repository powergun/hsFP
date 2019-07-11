#!/usr/bin/env stack runghc

-- this is the state monad example taken from book
-- haskell cookbook L3184

import Prelude hiding (Either(..))
import Data.Functor
import Control.Applicative
import Control.Monad

-- s: state
-- a: computation result
data State s a = State {
  runState :: s -> (a, s)
}

demoStateMonadCreation :: IO ()
demoStateMonadCreation = do
  print $ (runState (State (\s -> (1, 1))) 0)

instance Functor (State s) where 
         -- remember this is the type def, s is a type
  fmap f (State stateFunc) =
    let nextStateFunc s =
          let (xa, s1) = stateFunc s
          in (f xa, s1)
    in State nextStateFunc

demoStateAsFunctor :: IO ()
demoStateAsFunctor = do
  let sa = State (\s -> (1, s))
      f n = n * 10 + 1
      sb = fmap f sa
  print (runState sb 0)

instance Applicative (State s) where
  pure x = 
    State (\s -> (x, s))
  sf <*> sa =
    let stateFunc s =
          let (f, s1) = runState sf s
              (a, s2) = runState sa s1
          in (f a, s2)
    in State stateFunc

demoStateAsApplicative :: IO ()
demoStateAsApplicative = do
  let sa = (pure 1) :: State Int Int
      f n = n * 100 + n * 10 + 1
      sf = State (\s -> (f, s))
      sb = sf <*> sa
  print (runState sb 0)

instance Monad (State s) where
  return = pure
  -- f :: a -> m b
  -- L3267: monadic bind
  sa >>= f =
    let stateFunc s =
          let (a, s1) = runState sa s
              sb = f a
              (b, s2) = runState sb s1
          in (b, s2)
    in State stateFunc

demoStateMonad :: IO ()
demoStateMonad = do
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
  print $ runState sb 0
  print $ runState sc 0

get :: State s s
get =
  let stateFunc s = (s, s)
  in State stateFunc

demoGetState :: IO ()
demoGetState = do
  let sa = return 100
      sb = do
        x <- sa
        s <- get
        -- verify that I can make use of the state (-100)
        return (x + s)
  print $ runState sb (-100)

put :: s -> State s ()
put newState =
  State $ \_ -> ((), newState)

demoPutState :: IO ()
demoPutState = do
  let sa = return 1
      sb = do
        x <- sa
        s <- get
        put (x * 100 + s)
        -- not computing nothing, return the input (which is 1 
        -- defined in "sa = return 1") as it is
        return x
  print $ runState sb (1)

main :: IO ()
main = do
  demoStateMonadCreation
  demoStateAsFunctor
  demoStateAsApplicative
  demoStateMonad
  demoGetState
  demoPutState
