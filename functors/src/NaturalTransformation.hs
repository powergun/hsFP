{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

module NaturalTransformation (demo) where

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing  = []
maybeToList (Just a) = [a]

demo :: IO ()
demo = do
  print [maybeToList (Just 10), maybeToList Nothing]
