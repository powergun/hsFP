{-# LANGUAGE UnicodeSyntax #-}

module Contravariants (demo) where

import           Control.Applicative
import           Data.Functor.Contravariant (Contravariant (..))
-- show
newtype Predicate a = Predicate { getPredicate ∷ a → Bool }

instance Contravariant Predicate where
    contramap g (Predicate p) = Predicate (p . g)

veryOdd ∷ Predicate Integer
veryOdd = contramap (`div` 2) (Predicate odd)

demo ∷ IO ()
demo = print $ getPredicate veryOdd <$> [0, 1, 2, 3, 4, 5]
