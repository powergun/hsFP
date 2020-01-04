{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DemoQuickCheckMonoidLaw (demo) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

data Bull a = Bull a
              deriving (Eq, Show)
type BullInt = Bull [Int]

instance Arbitrary BullInt where
  arbitrary = oneof [return $ Bull [1 :: Int], return $ Bull [2 :: Int]]

instance Semigroup a => Semigroup (Bull a) where
  (<>) (Bull x) (Bull y) = Bull ((<>) x y)
instance Monoid a => Monoid (Bull a) where
  mappend = (<>)
  mempty = Bull mempty

instance Eq a => EqProp (Bull a) where
  (=-=) = eq

demo :: IO ()
demo = do
  quickBatch (monoid (Bull ([1] :: [Int])))
