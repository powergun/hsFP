#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

range_() {
    ghci <<HASKELL
a = [1..10]
show a
lessThanFive x = x < 5
filter lessThanFive a
filter (\x -> x < 5) a
filter (< 5) a
map (* 2) $ filter (< 5) a
HASKELL
}

range_
