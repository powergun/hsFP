#!/usr/bin/env stack runghc

-- programming haskell L2972
-- if a new type has a single constructor with a single arg
-- then it can also be declared using the newtype mechanism

-- type, data vs newtype
-- using newtype rather than type means that Nat and Int are
-- different types rather than synonyms, and hence the type
-- system of Haskell ensures that they can not accidentally
-- be mixed up in our programs
-- using newtype rather than data brings an efficiency benefit
-- because newtype constructors such as N do not incur any cost
-- when programs are evaluated as they are automatically
-- removed by the compiler once type checking is completed

-- in summary using newtype helps improve type safety without
-- affecting performance

-- real world haskell P/84
-- type BookRecord = (BookInfo, BookReview)
-- type works like typedef in C/C++

-- real world haskell P/196
-- type keyword gives us another way of referring to a type, like
-- a nickname for a friend
--   ^^^^^^^^
-- newtype exists to hide the nature of a type
--                   ^^^^^^^^^^^^^^^
-- as a user, we can not see its implementation detail

-- P/197
-- a newtype does not use automatic deriving to expose the
-- underlying type's implementation of a typeclass
-- we are free to either write a new instance or leave the
-- typeclass unimplemented

-- P/198
-- even though we use the value ctor for a newtype in the same
-- way as that of a type defined using the data keyword, all it
-- does is coerce a value between its "normal" type and its
-- newtype type
-- in other words, when we apply the N ctor in an expr, we
-- coerce an expression from type Int to type NewTypeInt as
-- far as we and the compiler are concerned, but absolutely
-- nothing occurs at runtime.

-- \\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\
-- haskell first principles P/607
-- newtype vs data
-- using newtype constrains the datatype to having a single
-- unary data constructor and newtype guarantees no additional
-- runtime overhead in "wrapping" the original type
--
-- why you might use newtype
-- 1) signal intent: wrapping type
-- 2) type safety: avoid mixing up many values of the same repr
-- 3) add different typeclass instances to a type that is
--    otherwise unchanged in repr

-- instance Num a => Monoid (Product a)
-- the instances say that we can use Sum or Product values
-- as a monoid as long as they contain numeric values
-- <> is mappend

main :: IO ()
main = do
  print 1
