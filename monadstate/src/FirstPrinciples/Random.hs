module FirstPrinciples.Random
  ( demo
  )
where

import           System.Random

-- source: first principles P/897
-- explore:
-- import System.Random
-- sg = mkStdGen 0 
-- radom sg :: (Char, StdGen)
-- ('\589059',40014 40692)
ran = random :: StdGen -> (Char, StdGen) -- -> (Int, StdGen)
rans count sg =
  let init   = ([], sg)
      range_ = replicate count 0
      f a b = (fst a ++ [fst . ran . snd $ a], snd . ran . snd $ a)
  in  fst $ foldl f init range_

data Die = DieOne
         | DieTwo
         | DieThree
         | DieFour
         | DieFive
         | DieSix
         deriving (Eq, Show)

demo :: IO ()
demo = do
  print . take 6 $ rans 5 (mkStdGen 123)
  print . take 6 $ rans 10 (mkStdGen 456)
  print . take 6 $ rans 5 (mkStdGen 1)
