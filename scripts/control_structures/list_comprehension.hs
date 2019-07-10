#!/usr/bin/env stack runghc

-- programming haskell L1513
-- | is read as such that
-- <- is read as is drawn from
-- expression x <- [1..5] is called a generator
-- a list comprehension can have more than one generator, with 
--   successive generators being separated by commas

-- motivation
-- how to call multiple functions in main
-- and display their outputs?
-- see
-- https://stackoverflow.com/questions/22964750/are-there-ways-to-call-two-functions-one-just-after-another-in-purely-function

foo :: Int -> [Int]
foo n = [ x ^ 2 | x <- [0..n] ]

bar n = n * 2

demo :: IO ()
demo = do
    print "demo nothing"
    print 1

demoPairing :: IO ()
demoPairing = do
    print "demo pairing"
    print [(x, y) | y <- [4, 5], x <- [1, 2, 3]]

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]
isPrime :: Int -> Bool
isPrime n = factors n == [1, n]
primes :: Int -> Int -> [Int]
primes from to = [ x | x <- [from .. to], isPrime x ]
find :: Eq a => a -> [(a, b)] -> [b]
find key table = [ v | (k', v) <- table, key == k' ]

demoGuards :: IO ()
demoGuards = do
    print "demo guards - is 1531 a prime?"
    print $ isPrime 1531
    print $ primes 3412 3531
    print $ find 't' [('t', 1), ('h', 2), ('e', 3), ('r', 5)]

main :: IO ()
main = do
    putStrLn $ show $ foo 6
    putStrLn $ show $ bar 32
    demo
    demoPairing
    demoGuards
