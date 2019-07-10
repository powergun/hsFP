#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

force_the_type_of_a_var() {
    ghci <<HASKELL
123 :: Float
-- (123 :: Int) + (12 :: Float)
-- error: try to add values of two different sources together
:type (+)
HASKELL
}

force_the_type_of_a_var
