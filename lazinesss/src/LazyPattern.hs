module LazyPattern (demo) where

compute :: (Int, Int) -> Int
compute ~(a, b) = 5

demo :: IO ()
demo = do
  print "//// lazy pattern: unless a binding is really requested"
  print $ compute undefined
