module ParserMonadV1
  ( Parser(..)

  ) where

newtype Parser a = Parser { parse :: String -> [(a, String)] }
