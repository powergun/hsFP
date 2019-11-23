module RecordSyntax
  ( Person(..)
  )
where

-- Person value constructor's type signature is still
-- λ> :t Person
-- Person :: String -> Int -> Person
-- 
-- Record syntax does not change how value ctor works
data Person = Person
  { name :: String
  , age :: Int
  }
