#!/usr/bin/env stack runghc

-- motivation:
-- I need a smart toStr() function that works with different
-- types; for Char and [Char] (String) types, toStr() simply
-- return the argument, whereas for other showable types it 
-- returns (show arg); for non-showable types it returns "..."

-- I found a post that explains a hack using typeclass
-- https://www.reddit.com/r/haskell/comments/5mbblu/what_i_really_miss_in_haskell_function_overloading/
-- here is the working example

{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances, OverlappingInstances, 
             FlexibleInstances, TypeSynonymInstances #-}

-- https://www.reddit.com/r/haskell/comments/5mbblu/what_i_really_miss_in_haskell_function_overloading/

class F w z where
  f :: w -> z

instance F Int (IO ()) where
  f = print

instance F String (IO ()) where
  f = print . show

instance F Char (IO ()) where
  f = print . show . show . show

instance F [a] ([b] -> Int -> IO ()) where
  f _ _ = print

demoCallOverloadedFunctions :: IO ()
demoCallOverloadedFunctions = do
  f (5 :: Int) :: IO ()
  f "thereis" :: IO ()
  f 'c' :: IO ()
  f ([1,2,3] :: [Double]) ([3,4,5] :: [Float]) (7 :: Int) :: IO ()

-- how can toStr work with different showable types?
-- i.e. for [Char] and Char, do not call "show" 
-- toStr :: (Show a) => a -> String
-- toStr v = show v
-- see :
-- https://stackoverflow.com/questions/968198/haskell-show-screwed-up
-- also:
-- https://stackoverflow.com/questions/12102874/haskell-suppress-quotes-around-strings-when-shown

-- However, as shown by the LANGUAGE pragmas, this is not very desirable.
class ToString a where
  toString :: a -> String

instance ToString String where
  toString = id

instance Show a => ToString a where
  toString = show

demoToStr :: IO ()
demoToStr = do
  print 
    "//////// demo toStr //////////////////////////////////////"
  print $ toString "asd"
  print $ toString 'c'
  print $ toString ([1..3] :: [Int])

main :: IO ()
main = do
  demoCallOverloadedFunctions
  demoToStr
