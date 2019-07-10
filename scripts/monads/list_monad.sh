#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

use_monad() {
    local filename
    filename=$(create_source "/var/tmp/list_monad.hs" <<'HASKELL'
import Control.Monad

-- equivalent to map .. ..
mapped = do
    i <- [0..9]
    return (i * 2)

-- can also work with infinite list
mappedInf = do
    i <- [0..]
    return (i * 3)

-- filtering with guard clause
-- see Control.Monad
div2 x = x `mod` 2 == 0
filtered = do
    i <- [0..]
    guard (div2 i)
    return i

-- combine mapping and filtering in a single function
mappedAndFiltered = do
    i <- [0..]
    guard (div2 i)
    return (i * 5 + 1)
HASKELL
)
    ghci <<HASKELL
:load ${filename}
mapped
take 10 mappedInf
take 10 filtered
take 6 mappedAndFiltered
HASKELL
}

use_monad
