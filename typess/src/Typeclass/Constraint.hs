{-# LANGUAGE FlexibleInstances #-}

module Typeclass.Constraint (demo) where

data Container elem = Container elem
data Element val = Element val

class DoFoo a where
  doFoo :: a -> String

class DoBar a where
  doBar :: a -> String

instance Monad m => DoBar (Element (m a)) where
  doBar x = "bar"

instance DoBar elem => DoFoo (Container elem) where
  --     ^^^^^^^^^^ type elem, which is the type-param of Container
  --  must be an instance of DoBar
  doFoo x = "foo"

demo :: IO ()
demo = do
  let elem = Element [1]  -- list is a monad
      ctn = Container elem
  print $ doBar elem
  print $ doFoo ctn
