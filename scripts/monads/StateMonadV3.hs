module StateMonadV3 (
    State (..)
  , get
  , put
  ) where

import Data.Functor ((<$>))

data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State stateFunc) =
    -- let newStateFunc s =
    --       let (x, s') = stateFunc s
    --       in (f x, s')
    -- in State newStateFunc

    -- see haskell cookbook L3367
    -- MY NOTES:
    -- rewrite using an alternative definition inspired by 
    -- the state monad transformer

    -- see the difference between normal state monad and state
    -- monad transformer: the latter has a deeper structure therefore
    -- requires <$> instead of . to compose f into the new 
    -- state function
    State ( (\(xa, xs) -> (f xa, xs)) . stateFunc )

instance Applicative (State s) where
  pure x = State (\s -> (x, s))

  sf <*> sa =
    let newStateFunc s = 
          let (f, s') = runState sf s
              (x, s'') = runState sa s'
          in (f x, s'') 
    in State newStateFunc

instance Monad (State s) where
  return = pure
  sa >>= f =
    let newStateFunc s = 
          let (x, s') = runState sa s
              (x', s'') = runState (f x) s'
          in (x', s'')
    in State newStateFunc

get :: State s s
get = State (\s -> (s, s))

put :: s -> State s ()
put newState = State (\_ -> ((), newState))
