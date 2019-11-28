module Bifunctors where

import qualified Data.Bifunctor as DB

type GameSession = Either Int Int

demoFirstSecond :: IO ()
demoFirstSecond = do
  let gs :: GameSession
      gs = Left (-10)
      gs' :: GameSession
      gs' = Right (99)
  case (DB.first show gs) of
    Left v -> print $ "Left(error): " ++ v
    _ -> return ()
  case (DB.second show gs') of
    Right v -> print $ "Right(success): " ++ v
    _ -> return ()
  
demo :: IO ()
demo = do
  demoFirstSecond
