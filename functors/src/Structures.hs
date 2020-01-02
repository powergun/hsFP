module Structures (demo) where

-- function is structure too
-- source: first principles P/664
originalF = const 'p'
newF = fmap (\_ -> 'x') originalF
demoNewF = do
  -- the type of input to newF does not matter; its type sig is
  -- b -> Char
  print [newF 1000, newF [1, 2]]

-- f x = x - 1
-- the calculation itself is the structure (the mapping of x to f x);
-- fmap does not change the structure (the mapping of x to f x) but
-- its value - i.e. (* 2) is applied after the original calculation
originalCalc = \x -> x - 1
newCalc = (* 2) <$> originalCalc
demoNewCalc = do
  print [newCalc 0, newCalc 1]

originalListGen = \x -> [x, 1..3]
newListGen' = show <$> originalListGen
newListGen = ((return '1' ++) . show) <$> originalListGen
-- return '1' is equivalent to ['1'] or "1"
-- it makes use of the List monad
demoNewListGen = do
  print [newListGen' 0, newListGen 0]

demoFmapOverTupleAndEither = do
  let a = (1, 'a')
      b = Right 21 :: Either String Int
      c = Left "Error" :: Either String Int
  print $ fmap (\c -> [c] ++ "---") a
  print $ fmap (\i -> "---" ++ show i) b
  -- this won't type check: fmap only respects the last type argument
  -- which is Int
  -- print $ fmap ("!!" ++) c

demo :: IO ()
demo = do
  demoNewF
  demoNewCalc
  demoNewListGen
  demoFmapOverTupleAndEither

