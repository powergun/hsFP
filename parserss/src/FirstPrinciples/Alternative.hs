{-# LANGUAGE QuasiQuotes #-}

module FirstPrinciples.Alternative
  ( demo
  )
where

import           Text.RawString.QQ
import           Text.Trifecta
import           Control.Applicative

type NumberOrString = Either Integer String

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

parseNos :: Parser [NumberOrString]
parseNos = many unit
 where
  unit = do
    skipMany (oneOf "\n")
    u <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return u

demo :: IO ()
demo = print $ parseString parseNos mempty eitherOr
