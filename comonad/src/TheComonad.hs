{-# LANGUAGE DeriveFunctor #-}

module TheComonad where

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate
  (=>=) :: (w a -> b) -> (w b -> c) -> (w a -> c)
  f =>= g = g . extend f

data Product e a = Prod e a
                   deriving (Functor)

instance Comonad (Product e) where
  extract (Prod e a) = a
  duplicate p@(Prod e a) = Prod e p

compute :: Product [Int] a -> [a]
compute (Prod xs a) = replicate (length xs) a

demo :: IO ()
demo = do
  let p = Prod [1, 2] "IDKFA"
  print $ extract p

  print $ (compute =>= compute) p
