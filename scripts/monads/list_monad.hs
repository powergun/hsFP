#!/usr/bin/env stack runghc

-- inspired by A Gentle Introduction to Haskell, Version 98
-- https://www.haskell.org/tutorial/monads.html
-- 9.2  Built-in Monads

gentleIntro :: IO ()
gentleIntro = do
  print $ [(x,y) | x <- [1,2,3] , y <- [1,2,3], x /= y]
  print $ do x <- [1,2,3]
             y <- [1,2,3]
             True <- return (x /= y)
             return (x,y)
  print $ [1,2,3] >>=
            (\x -> [1,2,3] >>= 
              (\y -> return (x/=y) >>=
                (\r -> case r of True -> return (x,y)
                                 _    -> fail "")))

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook L2778
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

nexts :: Num a => a -> [a]
nexts x = do
  x : nexts (x + 1)

demoNexts :: IO ()
demoNexts = do
  print $ take 5 $ nexts 3

-- the product of two list
pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do
  x <- xs
  y <- ys
  return (x, y)

demoPairs :: IO ()
demoPairs = do
  print $ pairs [-1, 1] [1..5]

partitionC :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
partitionC f xs ys = 
  [(x, y) | x <- xs, y <- ys, f x y]

partitionM :: (a -> b -> Bool) -> [a] -> [b] -> [(a, b)]
partitionM f xs ys = do
  x <- xs
  y <- ys -- recall that the first bind leaves longer
          -- therefore one x (drawn from xs) is paired with
          -- an entire ys
  if f x y then
    return (x, y)
  else
    []

demoPartition :: IO ()
demoPartition = do
  let l1 = partitionC (\x y -> x `mod` y == 1) [-5..5] [1, 2, 3]
      l2 = partitionM (\x y -> x `mod` y == 1) [-5..5] [1, 2, 3]
  print $ l1 == l2

main :: IO ()
main = do
  gentleIntro
  demoNexts
  demoPairs
  demoPartition
