#!/usr/bin/env stack runghc

{-# LANGUAGE ScopedTypeVariables #-}

{-
WTF I got "No instance for (Applicative Wrapped)" error ????

https://stackoverflow.com/questions/31652475/defining-a-new-monad-in-haskell-raises-no-instance-for-applicative

Maybe I should point out more clearly that this is a recent thing? 
The code you posted used to work before, but with 
recent versions of GHC you'll get an error. It's a breaking change.

import Control.Applicative 
-- Otherwise you can't do the Applicative instance.
import Control.Monad (liftM, ap)

instance Functor <my type> where
  fmap = liftM

instance Applicative <my type> where
  pure  = return
  (<*>) = ap
-}

import Control.Applicative 
import Control.Monad (liftM, ap)

-- the following example comes from 
-- A Gentle Introduction to Haskell, Version 98
-- https://www.haskell.org/tutorial/monads.html

-- monads provide modularity!
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^
-- by defining an operation monadically, we can hide underlying 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- machinery in a way that allows new features to be incorporated 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- into the monad transparently 
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- make it simple: State is an Integer
type S = Int

data SM a = 
  SM (S -> (a,S))  -- The monadic type

app :: SM a -> S -> (a, S)
app (SM st) x = st x

instance Show (SM a) where
  show _ = "..."

instance Functor SM where
  fmap = liftM

instance Applicative SM where
  pure  = return
  (<*>) = ap

instance Monad SM where
  -- defines state propagation
  -- see also programming haskell P169-170 state monad for its
  -- great diagrams
  SM c1 >>= fc2         =  SM $ \s0 -> 
                            let (r, s1) = c1 s0 
                                SM c2 = fc2 r 
                            in c2 s1
  return k              =  SM (\s -> (k,s))

 -- extracts the state from the monad
readSM                  :: SM S
readSM                  =  SM (\s -> (s,s))

 -- updates the state of the monad
updateSM                :: (S -> S) -> SM ()  -- alters the state
updateSM f              =  SM (\s -> ((), f s)) 

-- run a computation in the SM monad
-- MY NOTE:
-- this is equivalent to the app function, introduced in 
-- programming haskell P169
runSM                   :: S -> SM a -> (a,S)
runSM s0 (SM c)         =  c s0

demoMaybe :: IO ()
demoMaybe = do
  print $ do
    Just 1 >>= \x -> return (x * 10)

demoRunSM :: IO () -- demoApp
demoRunSM = do
  print "//// demo runSM"
  print $ do 
    runSM 12 (SM (\n -> ("asd", n)))

demoReadSM :: IO ()
demoReadSM = do
  print 
    "//////// demo readSM (passing argument) //////////////////"
  let sm1 = readSM -- readSM turns 13 below into an argument
      sm2 = do     -- computes a new value from 13 thanks to readSM
        arg <- sm1
        return $ arg * 10
      smA = return "iddqd" :: SM String -- can not take argument (hardcoded)
      smB = do                          -- computes a new value from hardcoded string
        hardcodedStr <- smA
        return $ hardcodedStr ++ "__hardcoding_is_bad"
  print $ app sm2 13
  print $ app smB 0 -- the initial state has no use at all!
    
demoReturn :: IO ()
demoReturn = do
  print 
    "//////// demo Monad return ///////////////////////////////"
  print $ app (return (1 :: Int) :: SM Int) 0
  print $ app (return "IDDQD" :: SM String) 1
  --      ^^^ to unwrap the value and state from the context (Monad)

demoUpdateSM :: IO ()
demoUpdateSM = do
  print 
    "//////// demo updateSM ///////////////////////////////////"
  let sm1 = readSM
      sm2 = do
        arg <- sm1
        -- selectively update the state
        updateSM $ \state ->
          case (state > 55) of 
            True -> state * 5 + 1
            False -> state * (-5) + 1
        return $ arg * 10 + 1
  print $ app sm2 11
  print $ app sm2 120

main :: IO ()
main = do
  demoMaybe
  demoRunSM
  demoReadSM
  demoReturn
  demoUpdateSM

