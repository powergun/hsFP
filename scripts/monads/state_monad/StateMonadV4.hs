module StateMonadV4 
  ( State(..)
  , get
  , put 
  , runState
  ) where

-- any type for the value; some type for the state
data State s a = State {
  stateFunc :: s -> (a, s)
}

runState = stateFunc

get :: State s s
get = State { stateFunc = (\s -> (s, s)) }

put :: s -> State s s
put s = State { stateFunc = (\_ -> (s, s)) }

-- recall Brian Beckman's video on monad:
-- monad is to enable function composition to solve complex problem

-- !!!combine things!!!

-- compose small functions to create a complex function
-- therefore, fmap, <*>, >>= must be designed in a way that:
-- > allow sequencing 
-- > allow arbitrary functions to be put together (no strict 
--   interface requirement other than the type requirement)
-- > strict type requirement (a -> a or a -> b)
instance Functor (State s) where
  fmap f sa = 
    let newStateFunc s =
          let (a, s') = stateFunc sa s
          in (f a, s')
    in State { stateFunc = newStateFunc }

instance Applicative (State s) where
  pure a = State { stateFunc = \s -> (a, s) }
  sf <*> sa = 
    let newStateFunc s =
          let (f, s')  = stateFunc sf s
              (a, s'') = stateFunc sa s'
          in (f a, s'')
    in State { stateFunc = newStateFunc }

instance Monad (State s) where
  return a = State { stateFunc = \s -> (a, s) }
  sa >>= f =
    let newStateFunc s =
          let (a, s')  = stateFunc sa s
              sb       = f a
              (b, s'') = stateFunc sb s'
          in (b, s'')
    in State { stateFunc = newStateFunc }
