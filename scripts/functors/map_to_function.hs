#!/usr/bin/env stack runghc

-- inspired by learn you a haskell
-- http://learnyouahaskell.com/a-fistful-of-monads
-- We even saw how to map a function a -> b over other functions 
-- of type r -> a to get functions of type r -> b 

-- starting with classic "container-based" functor
data Foo a = Foo {
  thisA :: a
} deriving (Show)

instance Functor Foo where
  fmap f foo =
    Foo { thisA = f $ thisA foo }

demoContainerBasedFunctor :: IO ()
demoContainerBasedFunctor = do
  print $ thisA $ Foo { thisA = 132 }
  print $ fmap (\n -> n + 100) (Foo { thisA = 132 })
  print $ fmap (\s -> s ++ "_called") (Foo { thisA = "there" })

-- map over function
demoFMapOverFunction :: IO ()
demoFMapOverFunction = do
  -- head being the r -> a
  let r = fmap (\elem -> elem ++ "_called") head
  print $ r ["iddqd", "is", "a", "cow"]

{-
To answer this question of how to map a function over some data 
type, all we had to do was look at the type of fmap: 

    fmap :: (Functor f) => (a -> b) -> f a -> f b  

And then make it work for our data type by writing the appropriate 
Functor instance. 

instance Functor f where
  fmap f f1 = ...

-}

main :: IO ()
main = do
  demoContainerBasedFunctor
  demoFMapOverFunction
