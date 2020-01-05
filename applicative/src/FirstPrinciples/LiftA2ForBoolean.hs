module FirstPrinciples.LiftA2ForBoolean (demo) where

liftA2 f a b = f <$> a <*> b

(<||>) = liftA2 (||)

largerThan :: Int -> Bool
largerThan = (> 10)

cleanlyDividenBy :: Int -> Bool
cleanlyDividenBy = (== 0) . ((flip mod) 5)

demo :: IO ()
demo = do
  print $ ((largerThan) <||> (cleanlyDividenBy)) 123
