#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

match() {
    local filename
    filename=$(create_source "/var/tmp/match.hs" <<HASKELL
-- xs is a common term in Haskell. It means multiple x. Think of
-- it as the plural of "x"
-- without this case, will get runtime error, Non-exhaustive patterns
-- with this case, will get empty list error (deliberately thrown)
newHead [] = error "empty list"
newHead (x:xs) = x

-- the default type signature is newHead :: [p] -> p
-- p represents any type

newTail [] = error "empty list"
newTail (x:xs) = xs
HASKELL
)

    ghci <<HASKELL
:load ${filename}
newHead [100, 1, 2, 4, 5]
newHead []
newTail [100, 3, 1, 4, 5]
newTail []
HASKELL
}

match
