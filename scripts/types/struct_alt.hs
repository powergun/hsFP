#!/usr/bin/env stack runghc

-- define a type Colour that supports two representations

module ColorRGBCMYK (Colour) where
data Colour = RGB Int Int Int | CMYK Float Float Float Float deriving Show

r = RGB 10 20 30
c = CMYK 1.0 2.0 3.0 4.0

main :: IO ()
main = do
    print $ show r ++ show c
