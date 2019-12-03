{-# LANGUAGE BangPatterns #-}

module FoldStopEarly.UseEither
  ( foldTerminate
  ) where

-- source
-- https://tech.fpcomplete.com/haskell/tutorial/monad-transformers

-- rely on the Either machinary
-- b: comparison rhs type
-- a: element type
-- no longer have to explicitly deal with an exit case:
-- binding with a Left value automatically terminates the loop
foldTerminate :: (b -> a -> Either b b) -> b -> [a] -> b
foldTerminate f accum0 list0 =
  either id id (go accum0 list0)
  where
    go !accum rest = do
      (x, xs) <- case rest of
                   []   -> Left accum
                   x:xs -> Right (x, xs)
      accum' <- f accum x -- termination point, accum' is Left
      go accum' xs

