{-# LANGUAGE DeriveFunctor #-}

module TheProduct where

data Product e a = Prod e a
                   deriving (Functor)

(=>=) :: (Product e a -> b) -> (Product e b -> c) -> (Product e a -> c)
(=>=) f g = \(Prod e a) -> let b = f (Prod e a)
                               c = g (Prod e b)
                           in c
extract (Prod e a) = a

compute :: Product [Int] a -> [a]
compute (Prod xs a) = replicate (length xs) a

demo :: IO ()
demo = do
  let p = Prod [1, 2] "IDDQD"
  print $ extract p

  print $ (compute =>= compute) p
