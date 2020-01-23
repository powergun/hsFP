module FirstPrinciples.Fizzfuzz (demo) where

import           Control.Monad             (mapM_)
import           Control.Monad.Trans.State

fizzfuzz :: Integer -> String
fizzfuzz n | n `mod` 15 == 0 = "fizzfuzz"
           | n `mod` 5 == 0  = "fizz"
           | n `mod` 3 == 0  = "fuzz"
           | otherwise       = show n

compute :: Integer -> State [String] ()
compute n = do
  xs <- get
  put $ (:) (fizzfuzz n) xs

demo :: IO ()
demo = do
  print $ execState (mapM_ compute [1..30]) []
