{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DemoQuickCheckApplicativeLaw (demo) where

import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

newtype TT a = TT a
               deriving ( Show, Eq, Functor, Arbitrary
                        , EqProp, CoArbitrary
                        )

instance Applicative TT where
  pure = TT
  (<*>) (TT f) (TT y) = TT (f y)

demo :: IO ()
demo = do
  let v :: TT (String, Char, Int)
      v = TT ("w", 'c', 1)
  quickBatch $ applicative v
