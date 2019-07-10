#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

partial() {
    ghci <<'HASKELL'
x `doSum` y = show x ++ show y
lPart = (5 `doSum`)
rPart = (`doSum` 6)
:type doSum
:type lPart
:type rPart
HASKELL

    # partial can be created with certain arguments
    # pre-populated
}

partial
