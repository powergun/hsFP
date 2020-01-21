module FirstPrinciples.IsomorphismCheck (demo) where

-- source: first principles P/898

-- this is the checker
-- this won't compile
-- type Iso a b = (a -> Maybe b, Maybe b -> a)
type Iso a b = (a -> b, b -> a)
newtype Sum a = Sum { getSum :: a }

demo :: IO ()
demo = do
  let test :: Iso a (Sum a)
      test = (Sum, getSum)
  print "passed"

