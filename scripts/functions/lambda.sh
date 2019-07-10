#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

lambda() {
    ghci <<HASKELL
:type \x -> x + 1
:type \x y -> x + y

pWordA = \s -> "(" ++ s ++ ")"
pWordB = ("(" ++) . (++ ")")
:type ("(" ++)

pWordA "there"
pWordB "there"
HASKELL
    # haskell lambda provides syntactical sugar to pass two or more arguments
    # lambda can be assigned to a variable; "valueness"

    # the second implementation uses section, a partial function
    # application 
}

lambda
