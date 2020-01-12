{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module FirstPrinciples.TypeComposition
  ( demo
  )
where

import           Data.Char
import           Control.Applicative            ( liftA2 )

newtype Compose f g a = Compose
  { getCompose :: f (g a)
  } deriving (Eq, Show)

demoTypeComposition :: IO ()
demoTypeComposition = do
  putStrLn "//// Type Composition examples"
  print $ Compose [[1]]

  -- f ~ []
  -- g ~ Maybe
  -- a ~ Int
  print $ Compose [Just 1, Nothing]

  let g = (1, )
      f = (, "idd")
  print $ Compose (f . g $ 1)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
  pure :: a -> Compose f g a
  pure = Compose . pure . pure

-- reference: https://wiki.haskell.org/Type_composition
  (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
  (Compose f) <*> (Compose a) = Compose (liftA2 (<*>) f a)

demoFunctor :: IO ()
demoFunctor = do
  let a1 = Compose [Just 1, Nothing]
  print $ (+ 10) <$> a1

demoApplicative :: IO ()
demoApplicative = do
  let a = pure 1 :: Compose [] Maybe Int
      f = pure (+ 100) :: Compose [] Maybe (Int -> Int)
  print $ f <*> a

demo :: IO ()
demo = do
  demoTypeComposition
  demoFunctor
  demoApplicative
