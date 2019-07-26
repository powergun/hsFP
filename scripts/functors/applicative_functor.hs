#!/usr/bin/env stack runghc

-- haskell design pattern P/110

import Control.Applicative

data Maybe' a = Just' a | Nothing'
  deriving (Show)

instance Functor Maybe' where
  fmap _  Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

instance Applicative Maybe' where
  -- haskell cookbook L2514
  -- pure() takes a value and creates a data type
  -- create the value in a new context
  -- example: pure 10 :: [Int] = [10]
  --          pure 10 :: Maybe Int = Just 10
  --          pure 10 :: Either String Int = Right 10
  pure f = Just' f
  Nothing' <*> _ = Nothing'
  _ <*> Nothing' = Nothing'
  (Just' f) <*> (Just' x) = Just' (f x)

demoApplicativeFunctor :: IO ()
demoApplicativeFunctor = do
  -- haskell design pattern P/110
  let x = pure (,) <*> Just' 2 <*> Just' 3
      y = Just' (.) <*> Just' (+2) <*> Just' (+3) <*> Just' 1
      z = (,) <$> Just' 2 <*> Just' 3 -- same effect as x's 
  print x
  print y
  print z

main :: IO ()
main = do
  demoApplicativeFunctor
