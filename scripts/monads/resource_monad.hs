#!/usr/bin/env stack runghc

-- A gentle introduction to haskell
-- https://www.haskell.org/tutorial/monads.html

import Control.Monad (liftM, ap, forM_)

-- type R to denote a computation

-- either returns a result of type a, or a suspended computation
-- of type R a, capturing the work done up to the point where
--              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- resources were exhausted
-- ^^^^^^^^^^^^^^^^^^^^^^^^
-- Resource type is used in the same manner as the state in the 
-- State Monad. 
type Resource = Int

-- computation instance: R some
-- computation state   : Resource, Left or Resource, Right
data R a = R (Resource -> (Resource, Either a (R a)))

instance Monad R where
  -- return() leaves the resources unchanged while moving v 
  -- into the monad
  -- MY NOTES:
  -- initiated in Done state
  return v = R (\r -> (r, (Left v)))

  -- c1 (computation instance)
  -- To combine two `resourceful` computations, c1 and fc2 (a 
  -- func producing c2), pass the initial resources into c1.
  -- The result will either be:
  -- -- a value v, and remaining resources, which are used to 
  --    determine the next computation (the call fc2 v)
  -- -- a suspended computation, pc1 and resources remaining 
  --    at the point of suspension
  R c1 >>= fc2 =
    R $ \r ->
      case c1 r of
        -- MY NOTES:
        -- Done state: create a new computation instance wrapping
        -- the giving new function fc2 and use it and r' - the 
        -- previous result to produce a new computation state
        (r', Left v) -> let R c2 = fc2 v
                        in  c2 r'
        -- The suspension must take the second computation into 
        -- consideration: pc1 suspends only the first computation,
        -- c1, so we must bind c2 to this to produce a suspension
        -- of the overall computation.
        -- MY NOTES:
        -- Suspend state: pass the current state back to the 
        -- caller (note r' the remaining resource)
        -- The new state must capture the work done; chaining and
        -- it can keep chaining forever if suspended
        (r', Right pc1) -> (r', Right (pc1 >>= fc2))

instance Applicative R where
  pure = return
  (<*>) = ap

instance Functor R where
  fmap = liftM

-- take a step unless no steps are available
step :: a -> R a
step v = c 
  where
    c = R (\r -> if r /= 0 then (r - 1, Left v)
                           else (r,     Right c))

demoStep :: IO ()
demoStep = do
  let (R f) = step "thereisacow"
      -- if no steps are available, the step function suspends
      -- the current computation (this suspension is captured in
      -- in c) and passes this suspended computation back into
      -- the monad
      -- MY NOTES:
      -- note how r' and r'' are 0 0 
      (r', Right (R f')) = f 0
      (r'', Right (R f'')) = f' 0
      (r_, Left v) = f'' 4
  print [r', r'', r_]
  forM_ (reverse [0..3]) 
        (\stepNum -> case f stepNum of
          (r', Left v)  -> print $ show stepNum ++ ": " ++ v
          (r', Right c) -> print $ show stepNum ++ ": " ++ "no") 

-- MY NOTES:
-- do not mix type with data/value ctor; this type def has NOTHING
-- to do with the value ctor pattern!!
inc :: R Int -> R Int
inc i = do 
  -- <- is necessary to pull the argument value out of the monad
  -- the type of iValue is Int instead of R Int
  -- MY IMPORTANT NOTES:
  -- R Int, the TYPE def is in effect here, <- i will pull the 
  -- instance of the TYPE out of Monad; it has NOTHING TO DO
  -- with the value ctor!!! (don't mistake that <- pulls the 
  -- function R -> (R, Either) out of the Monad)
  -- MY NOTES:
  -- recall (data_munging/IO/return) <- reverse the effect of 
  -- return; return v = R (\r -> (r, (Left v))); i is R (...)
  iValue <- i
  step (iValue + 1)

demoInc :: Int -> Int -> IO ()
demoInc n m = do
  let (R f) = inc (step m)
  case f n of
    (_, Left v) -> print v
    (_, Right c) -> print "no"

main :: IO ()
main = do
  demoStep
  demoInc 1337 -- any >0 Int
          3 -- initial value to increment; expect print out 4
