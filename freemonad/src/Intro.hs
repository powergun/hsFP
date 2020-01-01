module Intro where

data Free f a = Roll (f (Free f a))
              | Return a

instance Functor f => Functor (Free f) where
  fmap f (Roll x)   = Roll (fmap (fmap f) x)
  fmap f (Return x) = Return (f x)
instance Functor f => Applicative (Free f) where
  pure = return
instance Functor f => Monad (Free f) where
  return = Return
  Return m >>= k = k m  -- return m >>= k = k m
                        -- ex: return 10 >>= show
  Roll m >>= k = Roll (fmap (>>= k) m)

foldF :: Functor f => (f a -> a) -> Free f a -> a
foldF phi (Roll x) = phi $ fmap (foldF phi) x
foldF _ (Return x) = x
