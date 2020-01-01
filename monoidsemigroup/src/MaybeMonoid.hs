module MaybeMonoid (demo) where

import           Data.Bool   (bool)
import           Data.Monoid

first' :: (Num a, Eq a) => (a -> Bool) -> [a] -> Maybe a
first' predicate = getFirst . mconcat . (fmap toMaybe)
  where
    toMaybe x = First $ bool Nothing (Just x) (predicate x)

demo :: IO ()
demo = do
  print $ first' (== 0) [3, 1, 4, 15]
  print $ first' (/= 0) ([] :: [Int])
  print $ first' (>  1) [0, -10, 1, 0, 1.1]
