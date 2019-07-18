#!/usr/bin/env stack runghc

-- real world haskell P/371

import qualified Control.Monad.Writer as W
import qualified Control.Monad.State as S

demoProveListIsMonoid :: IO ()
demoProveListIsMonoid = do
  -- is list a monoid?? yes
  -- meaning that list can be used in W.WriterT
  print (mempty :: [String])
  print (([] :: [String]) `mappend` ["there"])
  print (["there", "is"] `mappend` ["a", "cow"])

log_ :: String -> S.State [String] ()
log_ str = do
  return ()

main :: IO ()
main = do
  let a = S.execState (log_ "asd") []
  print a
