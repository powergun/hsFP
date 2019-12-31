module TheStream where

import           Control.Comonad
import           Prelude         hiding (tail)

data Stream a = Cons a (Stream a)

instance Functor Stream where
  fmap f (Cons a as) = Cons (f a) (fmap f as)

instance Comonad Stream where
  extract (Cons a _) = a
  duplicate (Cons a as) = Cons (Cons a as) (duplicate as)

tail :: Stream a -> Stream a
tail (Cons a as) = as

fromList :: [a] -> Stream a
fromList (hd:tl) = Cons hd (fromList tl)

toList :: Stream a -> [a]
toList (Cons a as) = [a] ++ (toList as)

sumS :: Num a => Int -> Stream a -> a
sumS n (Cons a as) = if n <= 0 then 0 else a + sumS (n - 1) as

average :: Fractional a => Int -> Stream a -> a
average n stm = (sumS n stm) / (fromIntegral n)

movingAvg :: Fractional a => Int -> Stream a -> Stream a
movingAvg n = extend (average n)

demo :: IO ()
demo = do
  let s = fromList [1..]
      s' = fromList [1.0..]
  print . (take 4) . toList $ s
  print . (take 4) . toList . tail $ s
  print . (take 10) . toList . (movingAvg 100) $ s'
