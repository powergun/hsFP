#!/usr/bin/env stack runghc

import System.IO

-- motivation
-- demo do block as an alternative to then operator (>>)
-- see
-- https://en.wikibooks.org/wiki/Haskell/do_notation
-- The (>>) (then) operator works almost identically in do notation 
-- and in unsugared code

demoPutStrThen :: IO ()
demoPutStrThen =
  -- recall that, >> means the subsequent monad does not care about
  -- the value passed from the previous monad 
  putStr "there" >>
  putStr "is" >> 
  putStr "a" >>
  putStr "cow\n"

-- braces and semicolons are optional
-- we can chain any action as long as all of them are in the 
-- same monad
demoPutStrDo =
  do { putStr "there"
    ; putStr "is"
    ; putStr "a"
    ; putStr "cow\n"
  }

-- the bind operator >>= passes a value, namely the result of 
-- of an action of function, downstream in the binding sequence 
-- do notation assigns a variable name to the passed value using 
-- <-
demoBindValuesInDo :: IO ()
demoBindValuesInDo = 
  -- the curly braces and the semicolons are optional if every
  -- line of code is indented to line up equally
  -- with the explicit curly braces and semicolons, indentation
  -- plays no part and there is no danger
  do { h <- openFile("monad.md") ReadMode
    -- x1, x2 are the results of hGetLine, 
    -- the result is IO String, therefore x1 is bound to an String
    -- the two bound values are passed as arguments to print
    -- which create further action
    ; x1 <- hGetLine h
    ; x2 <- hGetLine h
    ; print x1
    ; print x2
    ; hClose h
  }

demoBindValuesInBind :: IO ()
demoBindValuesInBind =
  openFile("monad.md") ReadMode >>= 
    (\ h -> hGetLine h >>= 
      (\ x1 -> hGetLine h >>= 
        (\ x2 -> print x1 >> print x2 >> hClose h)))
        --       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ consume the values
        -- but do not generate more values, hence ">>" then

-- the fail method
-- see https://en.wikibooks.org/wiki/Haskell/Understanding_monads/Maybe
-- for how to create an example using the Maybe monad, case and Just 
canFail :: (Num a, Ord a) => a -> Maybe a
--         ^^^^^^^^^^^^^^ multiple constraints
canFail x | x > 0 = Just (x)
          | otherwise = Nothing
-- ^^^^^^^^^^^^^^^^^^ otherwise and Just (...)

main :: IO ()
main = do
  demoPutStrThen
  demoPutStrDo
  demoBindValuesInDo
  demoBindValuesInBind
