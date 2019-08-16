module ParserMonadV1
  ( Parser(..)
  , char
  , charUpper
  , string4
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

{-
programming haskell L5152
<*> applies a parser that returns a function to a parser that
returns an argument to give a parser that returns the result of
applying the function to the argument, and only succeeds if
all the components succeeds.
-}
instance Applicative Parser where
  pure v = Parser (\string -> [(v, string)])
  pf <*> p = Parser f'
    where
      f' string =
        case parse pf string of
          []         -> []
          [(f, out)] -> parse (f <$> p) out

{-
programming haskell L5167
the applicative machinary automatically ensures that this parser
fails if the input string is too short.
-}
string4 :: String -> Parser String
string4 [a, b, c, d] =
  pure f <*> char a <*> char b <*> char c <*> char d
  where
    f :: Char -> Char -> Char -> Char -> String
    f a b c d = [a, b, c, d]
string4 _ = pure ""

{-
programming haskell L5167
- >>= fails if the application of the parser p to the input string
  fails
- otherwise applies the function fp to the result value v to give
  another parser f v,
- which is then applied to the output string out that was produced
  by the first parser to give the final result
-}
instance Monad Parser where
  return = pure
  p >>= fp =
    Parser f
    where
      f string = case parse p string of
                  []         -> []
                  [(v, out)] -> parse (f v) out

