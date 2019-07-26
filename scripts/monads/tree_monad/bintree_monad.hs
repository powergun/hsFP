#!/usr/bin/env stack runghc

-- TODO: fix this broken bintree definition
-- TODO: see the proper definition in bintree_applicative 

data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            deriving (Show)

treeSUT :: Tree Int
treeSUT = 
  Node 
    (Node 
      (Leaf 1) 
      (Node 
        (Leaf 2) (Leaf 3))) 
    (Node 
      (Node 
        (Leaf 4) (Leaf 5)) 
      (Leaf 6))

instance Functor Tree where
  fmap f (Leaf x) =
    Leaf (f x)
  fmap f (Node l r) =
    Node (fmap f l) (fmap f r)

instance Applicative Tree where
  pure x = Leaf x
  (Leaf f) <*> (Leaf x) = Leaf (f x)
  (Leaf f) <*> (Node l r) = Leaf f
  -- haskell cookbook L2572
  -- MY NOTES:
  -- interesting behavior: lf applies to ALL the left branches
  -- rf applies to ALL the right branches !!
  -- Beautiful
  -- Node 
  -- left branches: (10 *) 
  -- (Node (Leaf 10) (Node (Leaf 20) (Leaf 30))) 
  -- right branches: (5 +) 
  -- (Node (Node (Leaf 9) (Leaf 10)) (Leaf 11))
  (Node lf rf) <*> (Node l r) = Node (lf <*> l) (rf <*> r)
  (Node lf rf) <*> (Leaf x) = Leaf x

-- see a related post:
-- https://stackoverflow.com/questions/6798699/monad-instance-for-binary-tree
-- MY NOTES:
-- f comes at different position for Applicative and Monad;
-- Applicative: f (a -> b) -> f a        -> f b
-- Monad      : m a        -> (a -> m b) -> m b
-- DO NOT MIX THEM UP!!!
instance Monad Tree where
  return x = Leaf x
  (Leaf x) >>= f =
    f x
  (Node l r) >>= f =
    Node (l >>= f) (r >>= f)

demoTreeMonadAsFunctor :: IO ()
demoTreeMonadAsFunctor = do
  print
    "//////// demo tree monad as functor (fmap) ///////////////"
  let sut = treeSUT
  print $ fmap (\n -> ":" ++ (show n)) sut
  print $ fmap (\n -> n `mod` 2) sut 
  print $ fmap (\n -> [n]) sut 

demoTreeMonadAsApplicative :: IO ()
demoTreeMonadAsApplicative = do
  print
    "//////// demo tree monad as applicative (<*>) ////////////"
  let sut = treeSUT
  print $ (Leaf (10 *)) <*> sut
  print $ (Node (Leaf (10 *)) (Leaf (5 +))) <*> sut

demoTreeMonad :: IO ()
demoTreeMonad = do
  print
    "//////// demo tree monad /////////////////////////////////"
  print $ treeSUT >>= (\n -> return (n + 100))
  print $ do
    n <- treeSUT
    return (n + 100)

-- MY NOTES:
-- this shows that void "()" can also be used as the Tree parameter
-- and becomes part of its Monad type
demoVoidTreeMonad :: IO ()
demoVoidTreeMonad = do
  print
    "//////// demo void-tree monad ////////////////////////////"
  print $ Leaf ()

main :: IO ()
main = do
  demoTreeMonadAsFunctor
  demoTreeMonadAsApplicative
  demoTreeMonad
  demoVoidTreeMonad
