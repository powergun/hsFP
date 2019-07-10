#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

# programming haskell P170
# the symbol $ denotes normal function application, defined by
# f $ x = f x
# MY NOTES:
# do not mistaken $ for <$> which is fmap

call_functions() {
    ghci <<HASKELL
square x = x ^ 2
squareSum x y = square x + square y
squareSum 3 4
f = let s = "hello" in putStrLn $ "(" ++ s ++ ")"
f
HASKELL
    # use $ to void typing parentesse
    # without $, must call putStrLn ("(" ++ s ++ ")")

}

call_functions
