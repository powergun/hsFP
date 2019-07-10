#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

lookup() {
    ghci <<'HASKELL'
let dict = [("one", 1), ("two", 2)]
lookup "one" dict
lookup "two" dict
lookup "ter" dict
HASKELL
}

lookup
