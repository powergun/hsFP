#!/usr/bin/env stack runghc

-- source
-- haskell cookbook

import qualified Control.Monad.State as S
import qualified Data.Map.Strict as M
import FibMonadV6

assert :: Bool -> IO ()
assert True = do
  return ()
assert False = do
  error "fail"

demoExecState :: IO () 
demoExecState = do
  print
    "//////// demo execState() - no result, only the last state"
  let mp = S.execState (fibWithState 30) M.empty
      Just v = M.lookup 30 mp
  assert $ 832040 == v

demoEvalState :: IO ()
demoEvalState = do
  print
    "//////// demo evalState() - throw away state; return result"
  let mp = S.execState (fibWithState 30) M.empty
  assert $ 514229 == (S.evalState (fibWithState 29) mp)
  assert $ 0 == (S.evalState (fibWithState (-12)) mp)

demoRunState :: IO ()
demoRunState = do
  print
    "//////// demo runState() - both the last state and result"
  let o = S.runState (fibWithState 15) M.empty
  assert $ 610 == (fst o)

main :: IO ()
main = do
  demoExecState
  demoEvalState
  demoRunState

