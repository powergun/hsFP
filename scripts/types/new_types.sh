#!/usr/bin/env bash

source "$(dirname $0)/../ghci_helper.sh"

create_new_type_source() {
    local filename="/var/tmp/directions.hs"
    cat >"${filename}" <<HASKELL

-- without deriving (recall Rust's derive decorator), one has to
-- implement all the required method in order to make the new type
-- comparable, displayable etc...

-- this variant: use alternate data constructor - is referred to
-- as sum type
-- there is also "product type" where data constructor takes args

data Compass = North | East | South | West
    deriving (Eq, Ord, Enum, Show)
HASKELL
    echo "${filename}"
}

new_types() {
    local filename
    filename=$(create_new_type_source)

    ghci <<HASKELL
type Port = Int
:info Port
type HostInfo = (String, Int)
:info HostInfo

:load ${filename}
North
North == North
East > North
succ North
HASKELL
}

new_types
