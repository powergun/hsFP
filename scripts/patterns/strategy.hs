#!/usr/bin/env stack runghc

-- haskell design pattern P/64
-- GOF strategy pattern

strategy fSetup fTeardown = do
    fSetup
    -- do common stuffs
    fTeardown
demoStrategy :: IO ()
demoStrategy = do
    print "demo strategy pattern"
    let f = strategy (* 2) (* 10)
    print $ f 2.0

main :: IO ()
main = do
    demoStrategy
