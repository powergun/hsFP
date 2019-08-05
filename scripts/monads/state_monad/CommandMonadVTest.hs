#!/usr/bin/env stack runghc

import           CommandMonadV2

assert :: Bool -> IO ()
assert True  = return ()
assert False = error "fail"

testOne cmd expected = do
  let oStr = execute cmd
  assert $ oStr == expected
  print oStr

main :: IO ()
main = do
  testOne "n(t)(n)*nn(9)*" "tttnnn|*|999|*|"
  testOne "nnnnnn" ""
  testOne "(o)*nnn" "ooo|*|"
