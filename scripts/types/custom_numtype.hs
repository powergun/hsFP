#!/usr/bin/env stack runghc

-- TODO: implement the rest of this example 
-- TODO: P/351 - P/356

-- real world haskell P/347
-- it might be nice to render numeric expressions as strings
-- P/348
-- Reverse Polish Notation (RPN)
-- RPN is a postfix notation that never requires parentheses
-- and is commonly found on HP calculators
-- RPN is a stack based notation
-- we push numbers onto the stack, and when we enter operations
-- they pop the most recent numbers off the stack and place the
-- result on the stack

-- P/349
-- if we want to make some custom behavior for the plus operator
-- possible then we will have to define a new type and make it
-- an instance of Num. This type will need to store an expression 
-- symbolically
-- we can therefore think of an expression as a sort of tree
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

-- P/350
-- defines the operations we will support
data Op = 
  Plus | Minus | Mul | Div | Pow
  deriving (Eq, Show)

{-
SymbolicMap will be an instance of Num. Define how the Num 
operations are handled over a SymbolicManip. This will implement
things like (+) for SymbolicManip

Because of the Num a constraint any Num can  be used for the a
So a full type may be something like SymbolicManip Int
-}
data SymbolicManip a = 
  Number a
  | Symbol String
  | BinaryArith Op (SymbolicManip a) (SymbolicManip a)
  | UnaryArith String (SymbolicManip a)
  deriving (Eq)

-- this converts 5 * 10 + 2 to SymbolicManip expression
instance Num a => Num (SymbolicManip a) where
  a + b = BinaryArith Plus a b
  a - b = BinaryArith Minus a b
  a * b = BinaryArith Mul a b
  negate a = BinaryArith Mul (Number (-1)) a
  abs a = UnaryArith "abs" a
  signum _ = error "signum is unimplemented"
  fromInteger i = Number (fromInteger i)

-- P/352
-- division is part of Fractional typeclass
instance (Fractional a) => Fractional (SymbolicManip a) where
  a / b = BinaryArith Div a b
  recip a = BinaryArith Div (Number 1) a
  fromRational r = Number (fromRational r)

instance (Floating a) => Floating (SymbolicManip a) where
  pi = Symbol "pi"
  exp a = UnaryArith "exp" a
  log a = UnaryArith "log" a
  sqrt a = UnaryArith "sqrt" a
  a ** b = BinaryArith Pow a b
  sin a = UnaryArith "sin" a
  cos a = UnaryArith "cos" a
  tan a = UnaryArith "tan" a
  asin a = UnaryArith "asin" a
  acos a = UnaryArith "acos" a
  atan a = UnaryArith "atan" a
  sinh a = UnaryArith "sinh" a
  cosh a = UnaryArith "cosh" a
  asinh a = UnaryArith "asinh" a
  acosh a = UnaryArith "acosh" a
  atanh a = UnaryArith "atanh" a

-- P/353
-- bare symbols and numbers are rendered bare
-- bin-ar is rendered with the two sides plus the op
prettyShow :: (Show a, Num a) => SymbolicManip a -> String
prettyShow (Number x) = show x
prettyShow (Symbol x) = x
-- prettyShow (BinaryArith op a b) =
--   let pa = simpleParen a
--       pb = simpleParen b
--       pop = op2str op
--   in pa ++ pop ++ pb
prettyShow (UnaryArith opstr a) =
  opstr ++ "(" ++ show a ++ ")"
prettyShow (BinaryArith op a b) =
  "(" ++ prettyShow a ++ op2str op ++ prettyShow b ++ ")"

op2str :: Op -> String
op2str Plus = " + "
op2str Minus = " - "
op2str Mul = " * "
op2str Div = " / "
op2str Pow = " ** "

-- simpleParen :: (Show a, Num a) => SymbolicManip a -> String
-- simpleParen (Number x) = prettyShow (Number x)
-- simpleParen (Symbol x) = prettyShow (Symbol x)
-- -- TODO: x@() syntax
-- -- review: scripts/variables/as_pattern.hs 
-- -- if the pattern after the @ matches, x will be bound to the 
-- -- entire text;
-- simpleParen x@(BinaryArith _ _ _) = "(" ++ prettyShow x ++ ")"
-- simpleParen x@(UnaryArith _ _) = prettyShow x

instance (Show a, Num a) => Show (SymbolicManip a) where
  show a = prettyShow a

-- P/350
-- integer numeric literals are internally treated as being 
-- wrapped in fromInteger, so 5 is just as valid as 
-- SymbolicManip Int as it as an Int 
demoCreateSymbolicManip :: IO ()
demoCreateSymbolicManip = do
  print ( (5 * (5 + 2 * (1 + 1))) :: SymbolicManip Int )


main :: IO ()
main = do
  demoCreateSymbolicManip
