module FoldStopEarly.UseEitherT (demo) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)

fold' :: Int -> [Int] -> ExceptT Int IO Int
fold' accum l =
  case l of
    [] -> return accum
    x:xs -> case x of
              -- if I instead do "return accum", the early termination will still produce a Right value!!!
              0 -> throwError accum
              _ -> fold' (accum + x) xs

process :: Int -> [Int] -> IO (Either Int Int)
process accum l =
  runExceptT (fold' accum l)

demo :: IO ()
demo = do
  print =<< process 0 [1, 2, 0, 3, 4]
  print =<< process 0 []
  print =<< process 0 [0]

