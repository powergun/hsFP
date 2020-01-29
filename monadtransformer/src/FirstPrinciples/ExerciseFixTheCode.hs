module FirstPrinciples.ExerciseFixTheCode where

import           Control.Monad
import           Control.Monad.Trans       (lift)
import           Control.Monad.Trans.Maybe

isValid :: String -> Bool
isValid v = '!' `elem` v

-- fix: add missing lift
maybeExcite :: MaybeT IO String
maybeExcite = do
  v <- lift getLine
  guard $ isValid v
  return v

-- fix: call runMaybeT to unwrap the MaybeT computation
doExcite :: IO ()
doExcite = do
  putStrLn "say something excite!"
  excite <- runMaybeT maybeExcite
  case excite of
    Nothing -> putStrLn "MOAR EXCITE"
    Just e  -> putStrLn ("Good, was very excite: " ++ e)
