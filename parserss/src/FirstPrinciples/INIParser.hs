{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module FirstPrinciples.INIParser (demo) where

import           Text.RawString.QQ (r)
import           Text.Trifecta

newtype Header = Header String deriving (Show, Eq)

testINI :: String
testINI = [r|; comment
[section]
host=wikipedia.org
alias=claw
|]

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

demo :: IO ()
demo = do
  demoParseBracketPair
