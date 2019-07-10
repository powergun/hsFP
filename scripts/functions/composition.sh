#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

function_composition() {
    ghci <<HASKELL
doubleIt x = x * 2
addTen x = x + 10
addTen (doubleIt 5)
(addTen . doubleIt) 5
(show . addTen . doubleIt) 5
f = show . addTen . doubleIt
:type f
map f [10, 11, 12, 13, 14]
HASKELL
    # use . operator to compose a new function from two given functions
    # this is equivalent to g(f n)
    # this require the result type and the input type chains properly
    # the "valueness" of a function - the newly composed function
    # will have its own type information
    # another advantage of this is that the resulting function can be
    # called in map on a list
}

function_composition
