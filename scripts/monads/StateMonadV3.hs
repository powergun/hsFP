module StateMonadV3 (
    State (..)
  , get
  , put
  ) where

data State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
  fmap f (State stateFunc) =
    let newStateFunc s =
          let (x, s') = stateFunc s
          in (f x, s')
    in State newStateFunc

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
