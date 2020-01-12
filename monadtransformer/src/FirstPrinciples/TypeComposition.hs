{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}

module FirstPrinciples.TypeComposition
  ( demo
  )
where

import           Data.Monoid
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

-- reference: https://github.com/scarvalhojr/haskellbook/blob/master/chapter25/section25.4-5-6.hs
-- can be hard to understand, but recall:
-- foldMap is to send an `f` to work on a structure 
-- f [ g [ a ] ]
-- this is slightly similar to fmap
instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose fga) = (foldMap . foldMap) f fga

-- newtype Compose' f a = Compose'
--   { getCompose' :: f a
--   } deriving (Show, Eq)
-- instance (Foldable f) => Foldable (Compose' f) where
--   foldMap f (Compose' a) = foldMap f a

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  -- (a -> f b) -> t a -> f (t b)
  --  a -> f b
  --  fga -> f fgb
  --              Compose fga -> f (Compose fgb)
  traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

demoFunctor :: IO ()
demoFunctor = do
  let a1 = Compose [Just 1, Nothing]
  print $ (+ 10) <$> a1

demoApplicative :: IO ()
demoApplicative = do
  let a = pure 1 :: Compose [] Maybe Int
      f = pure (+ 100) :: Compose [] Maybe (Int -> Int)
  print $ f <*> a

demoFoldable :: IO ()
demoFoldable = do
  let a :: Compose [] Maybe ([] Int)
      a = Compose $ replicate 3 (Just [1])
  print $ foldMap (Sum . length) a

demoTraversable :: IO ()
demoTraversable = do
  let a :: Compose [] Maybe ([] Int)
      a = Compose $ replicate 2 (Just [2])
  print =<< traverse print a  -- from [] Int -> IO () to IO [()]

demo :: IO ()
demo = do
  demoTypeComposition
  demoFunctor
  demoApplicative
  demoFoldable
  demoTraversable
