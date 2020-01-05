{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module DemoQuickCheckMonadLaw (demo) where

import           Control.Monad            (liftM, liftM2, liftM3)
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype DD a = DD a deriving (Eq, Show)

data Validation e a = L e
                    | R a
                    deriving (Functor, Eq, Show)

instance Applicative (Validation e) where
  pure = R
  L e <*> _ = L e
  R _ <*> L e = L e
  R f <*> R a = R (f a)

instance Monad (Validation e) where
  return = pure
  L e >>= _ = L e
  R s >>= f = case (f s) of e@(L _) -> e
                            other   -> other

instance (Arbitrary a) => Arbitrary (Validation String a) where
  arbitrary = oneof [ liftM L arbitrary
                    , liftM R arbitrary
                    ]
instance (Eq a, EqProp a) => EqProp (Validation String a) where
  (=-=) = eq

instance (Arbitrary a) => Arbitrary (DD a) where
  arbitrary = liftM DD arbitrary

instance (Eq a, EqProp a) => EqProp (DD a) where
  (=-=) = eq

demo :: IO ()
demo = do
  let v :: Maybe (String, String, Int)
      v = undefined
      val' :: Validation String (String, String, DD Int)
      val' = undefined
  quickBatch . monad $ v

  quickBatch . functor $ val'
  quickBatch . applicative $ val'
  quickBatch . monad $ val'
