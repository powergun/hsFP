#!/usr/bin/env stack runghc

-- programming haskell P164
-- note this example is also presented in 
-- Monads for functional programming.pdf (referenced by 
-- A gentle intro to haskell. version 98)

-- the motivation is to use multiple intro sources to climb the 
-- learning curve 

-- this example is more complex than the "maybe value example"
-- but not as complex as the DSL parser example in the gentle
-- introduction

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell cookbook L2740 has a great diagram showing the anatomy
-- of bind 
-- note how the scope of the first bind is available to the second
-- bind as well
-- L2752
-- note how the binding ensures that the previous computation 
-- has evaluated its result before moving to the next binding 
-- this ensures that the steps in a monadic computation are 
-- evaluated in a sequence
-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\

data Expr = Val Int 
          | Div Expr Expr
          deriving (Show)

eval :: Expr -> Maybe Int
eval (Val n) = Just n
eval (Div x y) = do
  n <- eval x
  m <- eval y
  case m of 0 -> Nothing
            _ -> Just (n `div` m)

-- no longer used because of the built-in Maybe Monad
-- (>>-) :: Maybe a -> (a -> Maybe b) -> Maybe b
-- mx >>- f =
--   case mx of
--     Nothing -> Nothing
--     Just x -> f x

demoSafeEval :: IO ()
demoSafeEval = do
  print $ eval (Div (Val 1) (Val 0))
  print $ eval (Div (Val 10) (Val 3))

{-
That is, we evaluate each of the expressions m1 ... mn in turn, 
and then combine their result values x1 ... xn by applying the 
function f. 

m1 >>= \x1 -> 
m2 >>= \x2 -> 
. 
. 
. 
mn >>= \xn ->
f x1 x2 ... xn

The definition of the >>= operator ensures that such an expression 
only succeeds if every component mi in the sequence succeeds.

Haskell provides a special notation for expressions of the above 
form, allowing them to be written in a simpler manner as follows: 

do x1 <- m1 
   x2 <- m2 
   . 
   . 
   . 
   xn <- mn 
   f x1 x2 ... xn 
-}

main :: IO ()
main = do
  demoSafeEval

