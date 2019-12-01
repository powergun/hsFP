module Bifunctors where

import qualified Data.Bifunctor as DB

-- inspired by
-- https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/profunctors
-- Haskell's Functor is only one of many functors in the mathematical
-- sense; It is in fact a covariant functor, meaning that fmap preserves
-- the direction of the arrows.

-- A bifunctor in the mathematical sense is a functor of two arguments;
-- three arguments would make trifunctors...

-- In Haskell this means a parametric type of kind * → * → *. Familiar
-- bifunctors include Either, (,) or even (->)

-- Note, GameSession must be an instance of Bifunctor in order
-- to be accepted by the first/second function
-- otherwise: No instance for (DB.Bifunctor GameSession)
-- Either value is Bifunctor instance
type GameSession = Either Int Int

demoEither :: IO ()
demoEither = do
  let gs :: GameSession
      gs = Left (-10)
      gs' :: GameSession
      gs' = Right (99)
  case (DB.first show gs) of
    Left v -> print $ "Left(error): " ++ v
    _      -> return ()
  case (DB.second show gs') of
    Right v -> print $ "Right(success): " ++ v
    _       -> return ()

demoTuple :: IO ()
demoTuple = do
  print $ DB.first show (1, 3)
  print $ DB.second show (1, 3)

demo :: IO ()
demo = do
  demoEither
  demoTuple
