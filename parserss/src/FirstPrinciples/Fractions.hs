{-# LANGUAGE OverloadedStrings #-}
module FirstPrinciples.Fractions
  ( demo
  )
where

import           Control.Applicative
import           Data.Ratio                     ( (%) )
import           Text.Trifecta
import           Data.Bool                      ( bool )

badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseFraction :: Parser Rational
parseFraction = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator can not be zero"
    _ -> return $ numerator % denominator

demo :: IO ()
demo = do
  putStrLn "\n"
  print $ parseString parseFraction mempty shouldWork
  print $ parseString parseFraction mempty shouldAlsoWork
  print $ parseString parseFraction mempty alsoBad
  print $ parseString parseFraction mempty badFraction
