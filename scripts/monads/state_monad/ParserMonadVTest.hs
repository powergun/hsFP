#!/usr/bin/env stack runghc

import           Data.Char

import           ParserMonadV1

assert :: Bool -> IO ()
assert True =
  return ()
assert False =
  error "fail"

testParsingOneChar :: IO ()
testParsingOneChar = do
  assert $ parse (char 't') "there" == [('t', "here")]
  assert $ null (parse (char 't') "iddqd")

testFunctor :: IO ()
testFunctor = do
  assert $ parse (charUpper 't') "there" == [('T', "here")]
  assert $ parse (toLower <$> char 'I') "ID" == [('i', "D")]

testApplicative :: IO ()
testApplicative = do
  assert $ parse (pure 'X') "there" == [('X', "there")]
  assert $ parse (pure toUpper <*> char 'x') "xxd" == [('X', "xd")]


main :: IO ()
main = do
  testParsingOneChar
  testFunctor
  testApplicative
