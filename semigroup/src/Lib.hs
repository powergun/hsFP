{-# LANGUAGE FlexibleInstances #-}

module Lib (someFunc, demo) where

newtype Foo b a = Foo a deriving (Eq, Show)
data Tag

instance Semigroup a => Semigroup (Foo Tag a) where
  (<>) (Foo x1) (Foo x2) = Foo $ (<>) x1 x2

someFunc = demo

demo :: IO ()
demo = do
  let foo1 = (Foo "idd") :: Foo Tag String
      foo2 = Foo "qd"
  print $ foo1 <> foo2
