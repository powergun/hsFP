module Singleton.Env (demo) where

-- source: https://github.com/thma/LtuPatternFactory
-- https://github.com/thma/LtuPatternFactory/blob/master/src/Singleton.hs

-- recall this applicative example: f takes an argument that is
-- threaded into + 1 and + 10; the results are then (+)
f = (+) <$> (+ 1) <*> (+ 10)

-- this works because (+ 1) and (+ 10) are of the same type:
-- Num a => a -> a
-- I can take a step further and make (+ 1), (+ 10) a composition
-- of logics with an effective type of `a -> a`
-- then f will be of type `a -> a`

demo = print $ f 1000
