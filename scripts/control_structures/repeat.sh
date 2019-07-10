#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

do_repeat() {
    ghci <<'HASKELL'
-- repeat [0..4]
-- this will create a list from range [0..3] for infinite times
-- the result is [[0,1,2,3], [0,1,2,3], [0,1,2,3] ....]

take 4 $ repeat [0..4]
-- create a list from range [0..4] for n times (n=4)
-- this is used to create 2d coords
HASKELL
}

do_repeat
