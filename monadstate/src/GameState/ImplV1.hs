module GameState.ImplV1
  ( play
  )
where

import qualified Control.Monad.State           as Ms

-- Example use of State monad
-- Passes a string of dictionary {a,b,c}
-- Game is to produce a number from the string.
-- By default the game is off, a C toggles the
-- game on and off. A 'a' gives +1 and a b gives -1.
-- E.g 
-- 'ab'    = 0
-- 'ca'    = 1
-- 'cabca' = 0
-- State = game is on or off & current score
--       = (Bool, Int)

type GameValue = Int
type GameState = (Bool, Int)

playGame :: String -> Ms.State GameState GameValue
playGame [] = do
  (_, score) <- Ms.get
  return score

playGame (x : xs) = do
  (on, score) <- Ms.get
  case x of
    'a' | on -> Ms.put (on, score + 1)
    'b' | on -> Ms.put (on, score - 1)
    'c'      -> Ms.put (not on, score)
    _        -> Ms.put (on, score)
  playGame xs

-- example: abcaaacbbcabbab
play :: String -> (Bool, Int)
play inputs = (st, sc)
 where
  startState = (False, 0)
  (st, sc)   = Ms.execState (playGame inputs) startState
