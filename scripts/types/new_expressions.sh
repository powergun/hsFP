#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

create_new_expression_source() {
    local filename="/var/tmp/expressions.hs"
    cat >"${filename}" <<HASKELL
data Expression = Number Int
                | Add Expression Expression
                | Substract Expression Expression
                deriving (Eq, Ord, Show)

-- this takes advantage of Haskell's pattern matching
calculate :: Expression -> Int
calculate (Number x) = x
calculate (Add x y) = (calculate x) + (calculate y)
calculate (Substract x y) = (calculate x) - (calculate y)
HASKELL
    echo "${filename}"
}

new_expression() {
    local filename
    filename=$(create_new_expression_source)

    ghci <<HASKELL
:load ${filename}
Number 1
Number 1 == Number 2

Add (Number 1) (Number 2)
Add (Number 1) (Substract (Number 10) (Number 5))
-- even though there is no implementation for the calculation
-- the type "Add" is still considered legit

:type calculate
calculate (Number 1)
calculate (Add (Number 1) (Number 10))
calculate (Add (Number 1) (Substract (Number 10) (Number 5)))
-- the last one sees Haskell recursively evaluates all the sub-
-- expressions before reaching the top level expression
HASKELL
}

new_expression
