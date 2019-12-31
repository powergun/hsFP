{-# LANGUAGE DeriveFunctor #-}

module ComonadLib where

import           Control.Comonad
data Product e a = Prod e a
                   deriving (Functor)

instance Comonad (Product e) where
  extract (Prod e a) = a
  duplicate p@(Prod e a) = Prod e p

compute :: Product [Int] a -> [a]
compute (Prod xs a) = replicate (length xs) a

demo :: IO ()
demo = do
  let p = Prod [1, 2] "BFG9000"
  print $ extract p

  print $ (compute =>= compute) p
