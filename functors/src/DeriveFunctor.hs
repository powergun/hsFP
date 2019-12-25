{-# LANGUAGE DeriveFunctor #-}

module DeriveFunctor (demo) where

newtype TT a = TT a deriving (Show, Functor)

demo :: IO ()
demo = do
  print $ fmap (+ 10) (TT 1)
