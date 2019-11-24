module TypeclassInstance
  ( demo
  )
where

data Probably a = Noway | Bingo a

-- (learn you a haskell)
-- without (Eq a) => typeclass constraint, if we use ==
-- on the contents of `Probably` we have no assurance
-- that what the `Probably` contains can be used with
-- Eq
-- therefore we have to add a class constraint:
-- with this instance decl, we say this:

-- we want all types of the form `Probably a` to be
-- part of the Eq typeclass, but only those types where
-- a is also part of Eq
-- * this is how Haskell would derive the instance too

instance (Eq a) => Eq (Probably a) where
  Bingo x == Bingo y = x == y
  Noway   == Noway   = True
  _       == _       = False

demo :: IO ()
demo = do
  let ret = Bingo 11 == Bingo 11
  print $ show ret
