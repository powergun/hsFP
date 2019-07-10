#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

list_comprehension() {
    local filename
    filename=$(create_source "/var/tmp/grid_monad.hs" <<'HASKELL'
grid2 = do
    i <- [0..4]
    -- this return is a standard expression that can be passed
    -- other functions
    return $ do
        j <- [0..4]
        return (i, j)
HASKELL
)
    ghci <<HASKELL
[ i * 2 | i <- [0..9] ]

-- with filter
[ i * 2 | i <- [0..9], i \`mod\` 2 == 0 ]

-- inf list
take 10 [ i * 5 + 1 | i <- [0..], i \`mod\` 2 == 0 ]

-- generate 2d array
row = 3
column = 3
og :: Show a => [a] -> IO ()
og = print . unlines . map show

-- naive approach - NOT WORKING!
grid = map (take column . repeat) [0..row - 1]
og grid

-- use list monad
:load ${filename}
putStrLn "//// use list monad"
show grid2

-- use list comprehension
putStrLn "//// use list comprehension"
grid3 = [ [ (row, col) | col <- [0..4] ] | row <- [0..4] ]
show grid3

-- use zip
-- use inf list to model arbitrary width and height
putStrLn "//// use zip"
repeat5 = take 5 . repeat
cols = repeat5 [0..4]
--     create a list from range [0..4], repeat this n times (n=5)
rows = map repeat5 [0..4]
--     for each i in [0..4], create a list of n copies (n=5) of i
-- zip is a special form of zipWith, which is
-- zipWith (,)
-- (,) is a function that combines two operands with commas in a
-- tuple
-- (,) 1, 2 ==> (1,2)
zipWith zip rows cols
HASKELL
}

list_comprehension
