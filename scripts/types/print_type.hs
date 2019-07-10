#!/usr/bin/env stack runghc

-- motivation:
-- how can I print the type info of an entity??? (not in ghci!!)
-- see: https://wiki.haskell.org/Determining_the_type_of_an_expression
-- (last section)
-- use typeOf from Data.Typeable

import Data.Typeable

type InfoP a =  Integer -- arg1
             -> Maybe Integer -- arg2
             -> a   -- ret

-- sizeP is really a value ctor!!
sizeP :: InfoP Integer
sizeP _ (Just sz) = sz
sizeP _ Nothing = -1

main :: IO ()
main = do
  print $ typeOf sizeP
  print $ sizeP 1123 (Just 1000)
  print $ sizeP 1123 Nothing