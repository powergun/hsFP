module Either.Bind (demo) where

-- show Either's early termination

runEither :: Either String Int -> Either String Int
runEither x = do
  v <- x  -- Left value will terminate
  return $ v + 1

demo :: IO ()
demo = do
  let a = Left "..."
      b = Right (-101)
  print $ runEither a
  print $ runEither b
