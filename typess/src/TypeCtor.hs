module TypeCtor
  ()
where


-- source:
-- learn you a haskell

-- data (Show a) => Probably a = Noway | Bingo a
-- this is legit syntax but ...
-- why I get this error in ghci?
-- "Illegal datatype context (use DatatypeContexts)"
-- see: 
-- https://stackoverflow.com/questions/18934882/haskell-line-of-code-not-compiling-illegal-datatype-context
-- 
-- see the section below to understand why it is not a common
-- practice to add typeclass constraints

-- `a` is the type parameter
-- because there is a type parameter involved, we call
-- Probably a type constructor
-- depending on what we want this data type to hold
-- when it is not Nothing, this type ctor can end up
-- producing a type of `Probably Int`
-- no value can have a type of `Probably` because that
-- is not a type, it is a type ctor;
-- in order for this to be a real type, it has to have
-- all its type parameters filled up
data Probably a = Noway | Bingo a

-- test in ghci
-- λ> a = Bingo 'a'
-- λ> :t a
-- a :: Probably Char

-- I don't have to use typeclass constraint, 
-- `(Show a, Show b) =>`, here

-- see a similar example:
-- monadtransformer/MaybeTrans
-- note m (the monadic type) has no constrain in the
-- ctor

-- see also:
-- http://learnyouahaskell.com/making-our-own-types-and-typeclasses
-- if we want to sum a list of numbers (requiring a 
-- type constrain here), we can specify later in the 
-- summing function that we specifically want a list
-- of numbers
data ShowAny a b = ShowAny
  { prefix :: a
  , suffix :: b
  }
show_ :: (Show a, Show b) => ShowAny a b -> String
show_ x = show (prefix x)


-- data (Show a) => Probably a = Noway | Bingo a
-- (learn you a haskell)
-- however it is a very strong convention in Haskell to never
-- add typeclass constraints in data declarations
-- because we don't benefit a lot but we end up writing more class
-- constraints, even when we don't need them
-- (put it simply)
-- constraints on data declaration force the function decl to carry
-- the same constraints 
-- without contraints on data decl, function decl is free to omit
-- the constraints
-- example: (Ord k) => on type and (Ord k) => on function
-- If Map k v had a type constraint in its data declaration, the
-- type for toList would have to be 
-- toList :: (Ord k) => Map k a -> [(k, a)], 
-- even though the function doesn't do any comparing of keys by order.

-- So don't put type constraints into data declarations even if it 
-- seems to make sense, because you'll have to put them into the 
-- function type declarations either way.
