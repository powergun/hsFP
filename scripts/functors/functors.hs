#!/usr/bin/env stack runghc

import Control.Applicative ((<$>))
import Data.Char (chr)

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook also explains functor pretty well
-- L2281
-- MY NOTE:
-- functor really is to give the container type a special ability 
-- such that: they abstract the inherent property of a data type
-- to adapt a function (a -> b) to itself
-- haskell cookbook L2376
-- Good functor diagram
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- programming haskell L4303
-- the idea of mapping a function over each element of a data struct
-- isn't specific to the type of lists, but can be abstracted further
-- to a wide range of parameterised types
-- the class of types that support such a mapping function are called
-- functors
-- NOTE: do I need to unlearn functors?
-- YES

-- haskell design pattern P/107
-- create a custom Maybe' type and make it an instance of Functor
data Maybe' a = Just' a | Nothing'
  deriving (Show)

instance Functor Maybe' where
  fmap _  Nothing' = Nothing'
  fmap f (Just' x) = Just' (f x)

demoCustomMaybe :: IO ()
demoCustomMaybe = do
  print "//// demo use custom Maybe class"
  -- fmap acts as a lifting function, taking our function up into
  -- the realm of the functor;
  -- the fmap also lifts function composition to the level of 
  -- functors
  print $ fmap show (Just' 7)

-- the fmap is to Functor what map is to the List type
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- real world haskell P/285
-- treeMap length tree
-- treeMap (odd . length) tree
-- haskell provides a typeclass to further generalize treeMap
-- this typeclass is named Functor, and it defines one function
-- fmap
-- (on lifting) it takes a function over ordinary values a -> b
-- and lifts it to become a function over containers f a -> f b
-- whtere f is the container type.
-- MY NOTE: the type of f is not limited to container types, 
-- it can be any function type; observe this:
-- fmap (+ 1) (+ 1) 1
-- 3
-- in this case f :: a -> a
demoPreludeFunctorInstances :: IO ()
demoPreludeFunctorInstances = do
  print "//// demo functor instances in Prelude (list and maybe)"
  print $ fmap length ["there", "is", "a", "cow"]
  print $ fmap length (Just "thereisacow")

-- real world haskell P/286-7
-- The definition of Functor imposes a few obvious restrictions 
-- on what we can do with fmap. 
-- For example, we can only make instances of Functor from types 
-- that have exactly one type parameter.
-- We can’t write an fmap implementation for Either a b or (a, b), 
-- for example, because these have two type parameters. We also 
-- can’t write one for Bool or Int, as they have no type parameters.

-- In addition, we can’t place any constraints on our type definition.
-- it would have required an Eq constraint to somehow get retroactively 
-- added to the signature of fmap.

-- omit type constraints from type definitions, and instead place 
-- them on the functions that need them.

demoFunctorOperator :: IO ()
demoFunctorOperator = do
  print "//// demo use functor operator (<$>)"
  print ( (+ 1) <$> [1, 2, 3] ++ [4, 5, 6] )

-- real world haskell P/289
-- rule 1. functors must preserve identity; fmap id gives back the 
--         identical value
-- rule 2. functors are composable. fmap (f . g) gives the same
--         result as (fmap f . fmap g)
-- observe
-- Prelude> f = fmap id (+ 1)
-- Prelude> :t f
-- f :: Num b => b -> b
-- Prelude> :t fmap
-- fmap :: Functor f => (a -> b) -> f a -> f b
demoFunctorRules :: IO ()
demoFunctorRules = do
  print "//// demo functor roles"
  let f = fmap id (show [1, 2])
  print $ f
  -- another way of looking at these two rules is that functors
  -- must preseve shape.
  -- the structure of a collection should not be affected by 
  -- a functor; only the values that it contains should change
  --            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- NOTE: given "f a", only the "a" part should change

-- NOTE: this is to document my understand on functor, see 
-- the original note in binparse / Parse
-- I have a great function g x = x * 2 but it only works with
-- Num type (compatible with * 2 operator), I want to extend its
-- functionality so that g Maybe x = Maybe (x * 2)
demoExtendFunctionality :: IO ()
demoExtendFunctionality = do
  print "//// demo extend functionality"
  let g x = x * 2
      f x = Just x
      t x = fmap f g
  print $ t Just 1

-- real world haskell P/291
-- Parse (Maybe a) is a functor within a functor. We thus have 
-- to lift a function twice to "get it into" the inner functor
newtype Parse a = Parse { state :: a } deriving (Show)
demoFunctorWithinFunctor :: IO ()
demoFunctorWithinFunctor = do
  print "//// demo functor within functor"
  let g :: Parse Int -> Int
      g parser = state parser
      f :: Int -> Char
      f i = chr i
      h :: Char -> String
      h c = "data: " ++ [c]
  -- given 111 (Int), return Char
  print $ (f <$> g) (Parse 111)
  -- given 111 (Int), return String
  print $ (h <$> (fmap f g)) (Parse 111)

-- haskell cookbook L2388
-- operator <$ :: a -> f b -> f a
-- no definition is required for this function as it can be 
-- defined using the const function
demoFunctorConst :: IO ()
demoFunctorConst = do
  print 
    "//////// demo functor op: <$ (using const) ///////////////"
  print $ fmap (const 1) [0..10]

main :: IO ()
main = do
  demoCustomMaybe
  demoPreludeFunctorInstances
  demoFunctorOperator
  demoFunctorRules
  demoExtendFunctionality
  demoFunctorWithinFunctor
  demoFunctorConst
