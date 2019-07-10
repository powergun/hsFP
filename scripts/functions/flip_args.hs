#!/usr/bin/env stack runghc

-- real world haskell P/252
-- flip function takes another function as argument and swaps
-- the order of its argument 
-- NOTE: compare this example to the partial function examples
-- such as 
-- algorithms/reduction/all_any.hs
-- regex/src/Glob.hs
-- the partial function pre-fills rhs
-- I can use `` syntax (prefill lhs or rhs) to achieve the same 
-- end result

shoot :: String -> String -> String
shoot x y = x ++ y
-- the following definition also works !!
-- x `shoot` y = x ++ y

demoFlip :: IO ()
demoFlip = do
  let foo = flip shoot "bang"
  print $ foo "boom"
  print $ (shoot "bang") "boom"
  -- normal partial lhs op ..
  print $ ("bang" `shoot`) "boom"
  -- equivalent to flip shoot "bang", prefill rhs: .. op rhs
  print $ (`shoot` "bang") "boom"

main :: IO ()
main = do
  demoFlip
