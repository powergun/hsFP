module DemoQuickCheckTraversableLaws
  ( demo
  )
where

import           Control.Monad                  ( liftM3 )
import           Test.QuickCheck
import           Test.QuickCheck.Classes
import           Test.QuickCheck.Checkers

-- replace SUT alias with the actual type
type SUT a = Either String a -- []

demoSimple :: IO ()
demoSimple = do
  let trigger = undefined :: SUT (Int, Int, [Int])
  quickBatch (traversable trigger)

-- First Principles P/862, chapter exercises: Tree Instance
data Tree a = Empty
            | Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = oneof
    [ return Empty
    , fmap Leaf arbitrary
    , liftM3 Node arbitrary arbitrary arbitrary
    ]

instance (EqProp a, Eq a) => EqProp (Tree a) where
  (=-=) = eq

instance Functor Tree where
  fmap f (Leaf x)     = Leaf (f x)
  fmap f Empty        = Empty
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- reference:
-- https://github.com/cwyang/haskell/blob/master/functor.hs
instance Foldable Tree where
  foldMap f Empty    = mempty
  foldMap f (Leaf x) = f x
  foldMap f (Node l x r) =
    (foldMap f l) `mappend` (f x) `mappend` (foldMap f r)

instance Traversable Tree where
  traverse f Empty    = pure Empty
-- recall that f is to provide the structure-of-truth while
-- Tree and its data ctors are to provide the inner structure
-- (refered to as collection in the case of list and other 
-- flat data structures)
  traverse f (Leaf x) = Leaf <$> f x
  traverse f (Node l x r) =
    pure Node <*> (traverse f l) <*> (f x) <*> (traverse f r)


demo :: IO ()
demo = do
  let trigger = undefined :: Tree (Int, Int, [Int])
  quickBatch (functor trigger)
  quickBatch (foldable (undefined :: Tree ([Int], [Int], [Int], Int, [Int])))
  quickBatch (traversable trigger)
