#!/usr/bin/env stack runghc

-- programming haskell L1126
-- "readable types" and how to use them

main :: IO ()
main = do
    -- this can NOT be a list due to its element type restriction
    print (a, b, c, d)
    where
        -- L1142, the use of :: resolves the type of the results 
        -- which would otherwise not be able to inferred by GHC
        a = read "False" :: Bool
        b = read "0x12" :: Int
        c = read "3.13" :: Float
        d = read "[\"asd\", \"asd\"]" :: [String]
