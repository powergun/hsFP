module ParserMonadV1
  ( Parser(..)
  , char
  , charUpper
  ) where

import           Data.Char (toUpper)

-- programming haskell L5122

newtype Parser a = Parser { parse :: String -> [(a, String)] }

char :: Char -> Parser Char
char c = Parser f
  where
    f [] = []
    f (x : xs) | x == c    = [(x, xs)]
               | otherwise = []

{-
recall Brian Beckman's video: functor and monad is to enable
us to compose complex function from simple functions in an
othorgnal way
-}

instance Functor Parser where
  fmap f p = Parser f'
    where
      f' string = case parse p string of
                    []         -> []
                    [(v, out)] -> [(f v, out)]

charUpper c = toUpper <$> char c
