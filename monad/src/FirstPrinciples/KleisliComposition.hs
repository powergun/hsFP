
module FirstPrinciples.KleisliComposition (demo) where

import           Control.Monad
import           Data.Bool     (bool)

compute :: (Ord a, Num a) => a -> Either String a
compute x = bool (Left "invalid") (Right (x ^ 2 + 2 * x + 1)) (x > 0)

cap :: (Ord a, Num a) => a -> a -> Either String a
cap max' x = return $ bool max' x (x < max')

demo :: IO ()
demo = do
  let f x = compute >=> compute >=> cap x
  print . (f 10) $ (-1)
  print . (f 100) $ 12
  print . (f 1000000) $ 12
