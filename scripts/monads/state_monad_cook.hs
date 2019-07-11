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

instance Functor (State s) where -- remember this is the type def, s is a type
  fmap f (State stateFunc) =
    let nextStateFunc s =
          let (xa, s1) = stateFunc s
          in (f xa, s1)
    in State nextStateFunc

demoStateMonadCreation :: IO ()
demoStateMonadCreation = do
  print $ (runState (State (\s -> (1, 1))) 1)

main :: IO ()
main = do
  demoStateMonadCreation
