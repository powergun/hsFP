{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FirstPrinciples.INIParser (demo) where

import           Control.Applicative ((<|>))
import           Data.Map            (Map, fromList)
import           Text.RawString.QQ   (r)
import           Text.Trifecta

newtype Header = Header String deriving (Show, Eq)
type Name = String
type Value = String
type Comment = [String]
data Section = Section Header (Map Name Value) deriving (Show, Eq)

parseBracketPair :: Parser a -> Parser a
parseBracketPair p = char '[' *> p <* char ']'

demoParseBracketPair :: IO ()
demoParseBracketPair = do
  let p = some letter
      p' = parseBracketPair p
  print $ parseString p' mempty "asd"
  print $ parseString p' mempty "[asd]"
  print $ parseString parseHeader mempty "[asd]"

parseHeader :: Parser Header
parseHeader = parseBracketPair (Header <$> some letter)

parseAssignment :: Parser (Name, Value)
parseAssignment =
  (,) <$> (some letter) <* (char '=') <*> (some (noneOf "\n")) <* skipEOL

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

demoParseAssignment :: IO ()
demoParseAssignment = do
  print $ parseString parseAssignment mempty "map=e1m1"

  let sut :: String
      sut = [r|map=e1m1

creatures=imp,gunner,archville
|]
  print $ parseString (some parseAssignment) mempty sut

parseComments :: Parser Comment
parseComments = do
  many (do _ <- char '#' <|> char ';'
           _ <- skipMany (oneOf " ")
           s <- many (noneOf "\n")
           skipEOL
           return s)

demoParseComments :: IO ()
demoParseComments = do
  let sut :: String
      sut = [r|# there is
; a cow e1me1
# 32
|]
  print $ parseString parseComments mempty sut

parseSection :: Parser Section
parseSection = do
  header <- (parseHeader <* skipEOL)
  kvs <- many (parseAssignment <* skipEOL)
  return $ Section header (fromList kvs)

demoParseSection :: IO ()
demoParseSection = do
  let testINI :: String
      testINI = [r|[section]
host=wikipedia.org
alias=claw
|]
  print $ parseString parseSection mempty testINI

demo :: IO ()
demo = do
  demoParseBracketPair
  demoParseAssignment
  demoParseComments
  demoParseSection

