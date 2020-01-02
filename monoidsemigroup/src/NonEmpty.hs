module NonEmpty (demo) where

data NonEmptyQ a = NonEmptyQ a [a] deriving (Ord, Eq, Show)
data NonEmptyL a = a :| [a] deriving (Ord, Eq, Show)

headQ :: NonEmptyQ a -> a
headQ (NonEmptyQ a _) = a

headL :: NonEmptyL a -> a
headL (a :| _) = a

demo :: IO ()
demo = do
  print . headQ $ NonEmptyQ "head" []
  print . headL $ "hd" :| []
