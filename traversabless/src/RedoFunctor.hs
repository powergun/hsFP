module RedoFunctor
  ( demo
  )
where

import           Data.Functor.Identity

fmap' f xs = runIdentity $ traverse (Identity . f) xs

demo :: IO ()
demo = print $ fmap' (+ 1) [1 .. 10]
