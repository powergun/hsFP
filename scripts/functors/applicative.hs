#!/usr/bin/env stack runghc

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook L2465
-- An applicative is a typeclass that is somewhere between a functor
-- and a monad; An applicative takes a functor one step further
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

-- haskell wikibook: Applicative Functors
-- https://en.wikibooks.org/wiki/Haskell/Applicative_functors

-- as useful as it is, fmap isn't much help if we want to apply
-- a function of two arguments to functorial values, for instance
-- how could we sum Just 2 and Just 3?
-- 
-- to have an operator with a type: f (a -> b) -> f a -> f b
-- to apply functions in the context of a functor, 
-- (+) <$> Just 2 <*> Just 3
-- recall <$> is fmap
-- ^^^^^^^^^^^^^^^^^^

-- <*> is one of the methods of Applicative, the type class of 
-- applicative functors - functors that support function application 
--                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- within their contexts 
-- ^^^^^^^^^^^^^^^^^^^^^
-- expressions such as (+) <$> Just 2 <*> Just 3 are said to be 
-- written in applicative style, which is as close as we can get 
-- to regular function application while working with a functor
-- (+) <$> Just 2 <*> Just 3
-- (+)     2          3

-- recall: functor offers a generalized way to map over a 
-- parameterized type; this parameterized type must take one and 
-- only one parameter
data Bean a = Bean {
  value :: a
} deriving (Show)

instance Functor Bean where
  fmap f bean = Bean { value = f (value bean) }

instance Applicative Bean where
  pure x = Bean {value = x}
  (Bean f) <*> bean = Bean { value = f (value bean) }

recapFunctor :: IO ()
recapFunctor = do
  print $ fmap (+1) (Bean { value = 1})

-- print ((+) <$> Just 2 <*> Just 3)
--        ^^^^^^^^^^^^^^
--      produce "Maybe f" so that this expression becomes
--               Maybe f <*> Maybe a
demoApplicative :: IO ()
demoApplicative = do
  let bean1 = (pure 20 :: Bean Int)
      bean2 = (pure (-20) :: Bean Int)
  print $ bean1
  --        20 * 10 + (-20)
  print $ (\n m -> n * 10 + m) <$> bean1 <*> bean2
  --      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  --      produces "Bean f" so that this expression becomes
  --                            (Bean f) <*> bean

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook L2473
-- MY NOTES:
-- the book's example also uses (,) (zipWith)
-- L2514
-- the applicative definition also implies that we can define 
-- an instance of an applicative for f only if f is also an
-- instance of a functor
multiplyLists :: Num a => [a] -> [a] -> [a]
multiplyLists xs ys =
  -- L2530
  -- remember the definition of Functor fmap
  -- if we take a function (a -> b -> c) and call fmap on a 
  -- functor instance we will get the following
  -- fmap :: (a -> b -> c) --> f a -> f (b -> c)
  -- resulting in f (b ->c)
  -- we can take f (b -> c) and apply it to f b using <*>
  -- and get f c 
  -- thus we can use a function such as (*) :: a -> a -> a and
  -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ 
  -- use it in the conjunction of <$> and <*> to apply more 
  -- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- complex things, such as multiplication on a couple of Maybe
  -- values
  (*) <$> xs <*> ys

-- haskell cookbook L2530
-- in the Applicative, we will encapsulate the function in the 
-- data type f (a -> b)... it is possible to carry more info
-- in the structure f 
-- for example one can consider f a as an operation carried out 
-- in parallel; f (a -> b) denotes that it needs to wait for 
-- the value to be produced by f a 
-- we can create a Applicative type that represents a thread 
-- pool and schedules f a on each one of them 

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- BUT just how is Haskell's List Applicative implemented ???
-- in particular, how is the definition of <*> for List ??
-- see
-- https://safiire.github.io/blog/2012/12/03/recreating-the-haskell-list-part-4-applicative-functors/
-- Since lists are applicative functors, you may combine a list 
-- of functions to a list of values and have it do the obvious 
-- thing, apply everything to everything, and then either mconcat 
-- or mappend the results into a single flat list. This just 
-- happens to be how <*> is implemented for a Haskell list, 
-- because it is really the only way that makes sense to implement 
-- it.
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

demoMultiplyLists :: IO ()
demoMultiplyLists = do
  print 
    "//////// demo multiplyLists() applicative ////////////////"
  print $ [1..4] `multiplyLists` []
  print $ [] `multiplyLists` [1..4]
  print $ [1..4] `multiplyLists` [-1, 1, -1]

main :: IO ()
main = do
  recapFunctor
  demoApplicative
  demoMultiplyLists
