module StateMonadV1 
  ( State(..)
  , get
  , put
  ) where

-- this is the state monad example taken from book
-- haskell cookbook L3184

-- s: state
-- a: computation result
data State s a = State {
  runState :: s -> (a, s)
}

instance Functor (State func) where 
         -- remember this is the type def (of data ctor), 
         -- func is a type
         -- also note that this is a concrete type, not the 
         -- parameterized State s a; "a" and "s" are not known
  fmap f (State stateFunc) =
    let nextStateFunc s =
          let (xa, s1) = stateFunc s
          in (f xa, s1)
    in State nextStateFunc

instance Applicative (State func) where
  pure x = 
    State (\s -> (x, s))
  sf <*> sa =
    let stateFunc s =
          let (f, s1) = runState sf s
              (a, s2) = runState sa s1
          in (f a, s2)
    in State stateFunc
    
instance Monad (State func) where
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

get :: State s s
get =
  let stateFunc s = (s, s)
  in State stateFunc

put :: s -> State s () -- the second type is the return value type
put newState =
  State $ \_ -> ((), newState)
