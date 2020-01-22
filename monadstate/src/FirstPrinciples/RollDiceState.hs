module FirstPrinciples.RollDiceState (demo) where

import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State
import           System.Random

data Die = Die1 | Die2 | Die3 | Die4 | Die5 | Die6 deriving (Eq, Show)

int2Die :: Integer -> Die
int2Die x = case x of
              1 -> Die1
              2 -> Die2
              3 -> Die3
              4 -> Die4
              5 -> Die5
              6 -> Die6
              _ -> undefined

rollDie :: State StdGen Die
rollDie = int2Die <$> state (randomR (1, 6))

roll3 :: State StdGen (Die, Die, Die)
roll3 = (,,) <$> rollDie <*> rollDie <*> rollDie

rollN :: Int -> State StdGen [Die]
rollN n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> [Int]
rollsToGetTwenty g = rollsTill 20 g

rollsTill :: Int -> StdGen -> [Int]
rollsTill maximum g =
    snd $ go [] 0 g
  where
    go :: [Int] -> Int -> StdGen -> (Int, [Int])
    go nums count gen =
      if (sum nums) >= maximum
        then (count, nums)
        else
          let (die, nextGen) = randomR (1, 6) gen
          in go (nums ++ [die]) (count + 1) nextGen

demo :: IO ()
demo = do
  let (d, s) = runState rollDie (mkStdGen 0)
      (d', s') = runState rollDie s
      (d'', s'') = runState rollDie s'
      (d3, s3) = runState roll3 s''
      (dn, s4) = runState (rollN 10) s3
  print (d, d', d'')
  print d3
  print dn
  print [ rollsToGetTwenty s
        , rollsToGetTwenty s'
        , rollsToGetTwenty s''
        , rollsToGetTwenty s3
        , rollsToGetTwenty s4
        ]
  print [ rollsTill 10 s
        , rollsTill 10 s'
        , rollsTill 10 s''
        , rollsTill 10 s3
        , rollsTill 10 s4
        ]
