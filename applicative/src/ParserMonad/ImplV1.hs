module ParserMonad.ImplV1
  ( Parser(..)
  , char
  , charUpper
  , string4
  , (<|>)
  , many
  , some
  , sat
  , string
  , integer
  , token
  )
where

import           Control.Applicative            ( Alternative
                                                , empty
                                                , many
                                                , some
                                                , (<|>)
                                                )
import           Data.Char                      ( isAlphaNum
                                                , isDigit
                                                , isSpace
                                                , toUpper
                                                )
-- programming haskell L5122

newtype Parser a = Parser { parse :: String -> [(a, String)] }

nextChar :: Parser Char
nextChar = Parser f
 where
  f []       = []
  f (x : xs) = [(x, xs)]

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
    f' string = case parse pf string of
      []         -> []
      [(f, out)] -> parse (f <$> p) out

{-
programming haskell L5167
the applicative machinary automatically ensures that this parser
fails if the input string is too short.
-}
string4 :: String -> Parser String
string4 [a, b, c, d] = pure f <*> char a <*> char b <*> char c <*> char d
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

L5167
because Parser is a monadic type, the do notation can be used
to sequence parsers together and process their result values
(use return/pure to inject the result value so that caller
can receive it)
-}
instance Monad Parser where
  return = pure
  p >>= fp = Parser f
   where
    f string = case parse p string of
      []         -> []
      [(v, out)] -> parse (fp v) out

{-
generally avoid using the the functorial fmap and applicative
<*> primitives on parsers. However, some users prefer writing
parsers in applicative style, and using an applicative approach
can sometimes be beneficial for optimising the performance of
  parsers.
-}

{-
programming haskell L5221
-}
instance Alternative Parser where
  empty = Parser $ const []
  lhs <|> rhs = Parser f
   where
    f string = case parse lhs string of
      []         -> parse rhs string
      [(v, out)] -> [(v, out)]

sat :: (Char -> Bool) -> Parser Char
sat predicate = do
  c <- nextChar
  case predicate c of
    True  -> return c
    False -> empty

string :: String -> Parser String
string str = case str of
  []       -> return []
  (x : xs) -> do
    char x
    string xs
    return (x : xs)

{-
programming haskell L5284
some vs many
the difference between these two repetition primitives is that
many permits zero or more applications of p, whereas some requires
at least one successful application
MY NOTE:
if many gets zero application, it returns an empty set as a legit
  output;
if some gets zero application, it returns an error

L5295
the default implementation of many and some are provided by
Alternative instance
-}

-- programming haskell L5321
space :: Parser ()
space = do
  many (sat isSpace)
  return ()

token :: Parser a -> Parser a
token p = do
  space
  t <- p
  space
  return t

integer :: Parser Int
integer = do
  let parser = some $ sat isAlphaNum
  t <- token parser
  return (read t :: Int)
