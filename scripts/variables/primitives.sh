#!/usr/bin/env bash

# Char Integer Int Float Double

source "$(dirname $0)/../ghci_helper.sh"

primitives() {
    ghci <<HASKELL
a :: Int; a = 1234
:sprint a
:type a
a
:sprint a
b :: Integer; b = 2 ^ 70
:sprint b
:type b
b
:sprint b
abs (-123)
HASKELL
    # the first call to sprint shows a = _
    # meaning unforced thunk - a has not been evaluated
    # after evaluating a, sprint shows the value held by a;

    # b is of type Integer which is not machine architecture specific
    # therefore b can hold really large value such as 2 of power of 70
    # if the same number were given to a, haskell would refuse and instead
    # give a 0

    # programming haskell L1173, negative numbers must be 
    # parenthesised
}

primitives
