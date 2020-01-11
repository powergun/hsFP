module FirstPrinciples.ReaderPractice
  ( main
  )
where

import           Control.Applicative
import           Data.Maybe              hiding ( fromMaybe )

import           Prelude                 hiding ( lookup
                                                , uncurry
                                                )
import           Data.Bool                      ( bool )

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

lookup :: Eq a => a -> [(a, b)] -> Maybe b
lookup x (p : ps) = bool (lookup x ps) (Just . snd $ p) (x == fst p)
lookup x []       = Nothing

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = lookup 3 $ zip x y

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = lookup 6 $ zip y z

-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = z' 4 -- lookup 4 $ zip x y

-- zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' = flip lookup $ zip x z

-- x1 make a tuple of xs and ys
x1 :: Maybe (Integer, Integer)
x1 = (,) <$> xs <*> ys

-- x2 make a tuple of ys and zs
x2 :: Maybe (Integer, Integer)
x2 = pure (,) <*> ys <*> zs

-- x3 takes one input and makes a tuple of the results of two
-- application of z' from above
x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 = (,) <$> z' <*> z'

uncurry :: (a -> b -> c) -> (a, b) -> c
uncurry f = f <$> fst <*> snd

summed :: Num c => (c, c) -> c
summed = uncurry (+)

-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = (&&) <$> (> 3) <*> (< 8)

-- give it a default value and a maybe value, if the maybe value
-- is a `Just a`, it will return the a value; if the maybe value
-- is a `Nothing`, it returns the default value instead
fromMaybe :: a -> Maybe a -> a
fromMaybe d x = case x of
  Just n  -> n
  Nothing -> d

sequA :: Integral a => a -> [Bool]
sequA = sequenceA [(> 3), (< 8), even]

s' = summed <$> ((,) <$> xs <*> ys)

-- fold the boolean conjunction operator over the list of results
--   of sequA (applied to some value)
-- apply sequA to s'; you will need fromMaybe
-- apply bolt to ys, you will need fromMaybe

main :: IO ()
main = do
  -- print $ sequenceA [Just 3, Just 2, Just 1]
  -- print $ sequenceA [x, y]
  -- print $ sequenceA [xs, ys]
  -- print $ summed <$> ((,) <$> xs <*> ys)
  -- print $ fmap summed ((,) <$> xs <*> zs)
  -- print $ bolt 7
  -- print $ fmap bolt z

  -- print $ sequenceA [(> 3), (< 8), even] 7

  print $ foldl (&&) True $ sequA 7
  print $ sequA $ fromMaybe 0 s'
  print $ bolt . fromMaybe 0 $ ys
