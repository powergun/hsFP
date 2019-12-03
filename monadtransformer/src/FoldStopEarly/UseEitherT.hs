module FoldStopEarly.UseEitherT (demo) where

import           Control.Monad.Except (ExceptT, runExceptT, throwError)
import           Data.Bool            (bool)

foldHardcoded :: Int -> [Int] -> ExceptT Int IO Int
foldHardcoded accum l =
  case l of
    [] -> return accum
    x:xs -> case x of
              -- if I instead do "return accum", the early termination will still produce a Right value!!!
              0 -> throwError accum
              _ -> foldHardcoded (accum + x) xs

type Predicate = Int -> Int -> Either Int Int

foldPredicate :: Predicate -> [Int] -> Int -> ExceptT Int IO Int
foldPredicate p l accum =
  case l of
    []   -> return accum
    x:xs -> either throwError (foldPredicate p xs) (p accum x)

process :: Int -> [Int] -> IO (Either Int Int)
process accum l =
  runExceptT (foldHardcoded accum l)

process' :: Int -> [Int] -> IO (Either Int Int)
process' accum l =
  let p :: Predicate
      p accum x = bool (Left accum) (Right $ accum + x) (x /= 0)
  in runExceptT $ foldPredicate p l accum

demo :: Int -> [Int] -> IO Int
demo accum l = do
  either return return =<< process' accum l
