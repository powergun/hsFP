#!/usr/bin/env stack runghc

import           ParserMonadV1

main :: IO ()
main = do
  let p = Parser (\_ -> [(' ', "")])
  print $ parse p ""
