#!/usr/bin/env stack runghc

import           CommandMonadV1

assert :: Bool -> IO ()
assert True  = return ()
assert False = error "fail"

main :: IO ()
main = do
  let oStr = execute "n(t)(n)*nn(9)*"
  assert $ oStr == "tttnnn|*|999|*|"
  print oStr
