module FirstPrinciples.Bifunctors (demo) where

import Data.Bifunctor
import qualified Data.Bifunctor                as DB

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


--- these exercises come from First Principles P/991

data Deux a b = Deux a b deriving (Show, Eq)

instance Bifunctor Deux where
  bimap f g = first f . second g
  first f (Deux a b) = Deux (f a) b
  second f (Deux a b) = Deux a (f b)

demoDeuxBifunctor :: IO ()
demoDeuxBifunctor = do
  let d = Deux 12 24
  print $ bimap (+ 10) (+ 100) d

demo :: IO ()
demo = do
  putStrLn "//// Bifunctor examples"
  demoEither
  demoTuple
  demoDeuxBifunctor
  print $ bimap (+ 10) (+ 100) (Left 0) 
  print $ bimap (+ 10) (+ 100) (Right 0) 