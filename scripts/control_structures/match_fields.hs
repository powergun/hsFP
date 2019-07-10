#!/usr/bin/env stack runghc

data Colour = RGB Int Int Int deriving Show

red :: Colour -> Int
red (RGB r _ _ ) = r

green :: Colour -> Int
green (RGB _ g _ ) = g

blue :: Colour -> Int
blue (RGB _ _ b ) = b

-- pattern matching can be nested
data Pixel = Pixel Int Int Int Colour
-- type ctor and data ctor can have the same name as they
-- are in different namespaces
pixelRed :: Pixel -> Int
pixelRed (Pixel _ _ _ (RGB r _ _)) = r

-- return different value depending on the number of
-- fields
-- can be useful to implement runtime type checking
data TrueColor = SRGB Int Int Int | CMYK Float Float Float Float deriving Show
colorModel :: TrueColor -> String
colorModel (SRGB _ _ _) = "RGB"
-- colorModel (CMYK _ _ _ _) = "CMYK"

-- use case of syntax
colorModel_ :: TrueColor -> String
colorModel_ c =
    -- non-exhaustive cases will cause a runtime error, instead
    -- of compile-time error, unlike in rust
    -- ghc has -Wincomplete-patterns (or simply -Wall) to catch
    -- such issue; also has -Werror
    case c of SRGB _ _ _ -> "RGB"
              CMYK _ _ _ _ -> "CMYK"

main :: IO ()
main = do

    -- let c = RGB 10 20 30
    -- print $ red c
    -- print $ green c
    -- print $ blue c

    -- let p = Pixel 10 10 10 (RGB 1 2 3)
    -- print $ pixelRed p

    let c = CMYK 1 2 3 4
    -- print $ colorModel c
    -- use case of syntax, the result is the same
    print $ colorModel_ c
