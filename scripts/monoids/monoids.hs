#!/usr/bin/env stack runghc

-- real world haskell P/360
-- in abstract algebra, there is a simple abstract structure
-- called a monoid
-- in order to be considered a monoid, an object must have two
-- properties:
-- an associative binary op a * (b * c)
-- an identity value, a * e == a and e * a == a
-- the rules for monoid don't say what the binary operator must
-- do, merely that such an operator must exist

-- haskell first principles P/604
demoListAsMonoid :: IO ()
demoListAsMonoid = do
  print $ mappend [1,2] [3,4] `mappend` (mempty :: [Int])
  print $ mconcat [[1,2],[3,4]]

-- haskell first principles P/605
-- None of the numeric types have a Monoid instance
-- summation and multiplicaion are monoidal but each type
-- should only have one unique instance for a given typeclass
-- ... it isn't clear if those should be added or multiplied
-- as a mappend operation

-- P/606
-- lists have more than one possible monoid, several other types
-- do as well; We usually enforce the unique instance rule by
-- using newtype to separate the different monoidal behaviors
-- "integers form a monoid under summation and multiplication"
-- "lists form a monoid under concatenation"

main :: IO ()
main = do
  demoListAsMonoid
