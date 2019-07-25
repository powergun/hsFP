#!/usr/bin/env stack runghc

-- read:
-- http://learnyouahaskell.com/syntax-in-functions
-- http://zvon.org/other/haskell/Outputsyntax/caseQexpressions_reference.html
-- https://www.haskell.org/tutorial/patterns.html

-- here demo the use of "do block"
-- however if there are multiple levels of do block (nested do
-- ) inconsistent use of braces and indentation will cause error
-- example: data_munging/fileIO/read_lines
demoCaseDoBlock :: Show a => [a] -> IO ()
demoCaseDoBlock x = do
  print "//// demo use case expression and do {}"
  case x of
    [x] -> do {
      print "1";
      print "one";
    }
    -- without { }; this is prefered notation
    x -> do
      print "n1, n2 .... n"
      print "all"

-- real world haskell P/106
-- the case construct lets us match patterns within an expr
canFail :: Int -> Maybe Int
canFail x =
  case (x <= 0) of
    True -> case (x == 0) of
              True -> Nothing
              False -> Just (-x)
    False -> Just x

canFailWithGuard :: Int -> Maybe Int
canFailWithGuard x
  | x == 0    = Nothing
  | x <  0    = Just (-x)
  -- real world haskell P/109
  -- "otherwise" is simply a variable bound to the value True
  -- that aids readability (if everything above fails, this is
  -- what gets evaluated)
  | otherwise = Just x

-- P/106
-- this function unwraps a maybe value, using a default if the 
-- value is Nothing.
-- NOTE the default value and the wrapped value must be of the 
-- same type (otherwise there will be two different return types)
fromMaybe :: String -> Maybe String -> String
fromMaybe defval wrapped = 
  -- the case keyword is followed by an arbitrary expression;
  -- the pattern match is performed against the result of this
  --                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  -- expression. 
  -- ^^^^^^^^^^
  -- real world haskell P/108
  -- pattern matching limits us to performing fixed tests of 
  -- a value's shape (value constructor)
  -- (meaning that I CAN NOT DO x < 0 sort of matching; I 
  -- need to use Guards; see guarded_equations)

  -- the of keyword signifies the end of the expression and 
  -- the beginning of the block of patterns and expressions
  case wrapped of
    Just value -> value
    Nothing -> defval

-- real world haskell P/108 
-- we refer  to a pattern that always succeeds as irrefutable 
-- plain variable names and the wild card _ are examples of 
-- irrefutable patterns

-- real world haskell P/161
-- how to use case expression in combination with guard clause
-- REMEMBER guard clause takes boolean value; case does pattern
-- matching
toUpper :: Char -> Char
toUpper c = case c `lookup` lcChars of
  Just uc -> uc
  Nothing | c == ' ' -> '_'
          | otherwise -> '*'
  where
    lcChars :: [(Char, Char)]
    lcChars = zip ['a'..'z'] ['A'..'Z']

demoToUpper :: IO ()
demoToUpper = do
  print "//// demo toUpper"
  print $ map toUpper "VAR iddqd = true"

demoCanFail :: IO ()
demoCanFail = do
  print "//// demo canFail()"
  print $ canFail (-23)
  print $ canFail 0
  print $ canFail 1342
  print $ fromMaybe "fail" Nothing
  print $ fromMaybe "fail" (Just "asd")
  print $ canFailWithGuard (-232)
  print $ canFailWithGuard (0)
  print $ canFailWithGuard (3242)

main :: IO ()
main = do
  demoCaseDoBlock [1]
  demoCaseDoBlock [1, 2]
  demoCanFail
  demoToUpper
