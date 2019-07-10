#!/usr/bin/env stack runghc

-- | where binding is similar to let binding
f x y = a + b
    where
        a = x
        b = y

main = print (1)
