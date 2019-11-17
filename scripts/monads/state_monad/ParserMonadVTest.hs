#!/usr/bin/env stack runghc

import           Control.Monad
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
  assert $ parse (string4 "id") "iddqd" == [("", "iddqd")]
  assert $ parse (string4 "idfa") "idfa-iddqd" == [("idfa", "-iddqd")]
  assert $ null (parse (string4 "1337") "ad")

testMonadCharChain :: IO ()
testMonadCharChain = do
  let cheatcode = do
        char 'i'
        char 'd'
        char 'd'
        return "invincible"
  assert $ parse cheatcode "iddqd" == [("invincible","qd")]

testAlternative :: IO ()
testAlternative = do
  let cheatcodes = do
        char 'i'
        char 'd'
        char 'd' <|> char 'k'
        return "cheats"
  assert $ parse cheatcodes "iddqd" == [("cheats","qd")]
  assert $ parse cheatcodes "idkfa" == [("cheats","fa")]

testCharPredicate :: IO ()
testCharPredicate = do
  assert $ parse (sat isDigit) "0x1" == [('0', "x1")]
  assert $ null (parse (sat isDigit) "abc")

testString :: IO ()
testString = do
  -- let parsers = mapM ["iddaf", "iddqd", "idkfa", "idclip"] $ \s -> do
  --                 string "idd" <|> string "idk" <|> string "idc"
  --                 return "1"
  let parsers = (string "idd" <|> string "idk" <|> string "idc") >> return "1"
  assert $ null (parse parsers "idfa")
  assert $ parse parsers "iddqd" == [("1", "qd")]

testManyP :: IO ()
testManyP = do
  assert $ parse (many $ sat isDigit) "1234abc" == [("1234", "abc")]
  assert $ parse (many $ sat isAlphaNum) "&^%@#" == [("", "&^%@#")]

testSomeP :: IO ()
testSomeP = do
  assert $ parse (some $ sat isDigit) "1234abc" == [("1234", "abc")]
  assert $ parse (some $ sat isAlphaNum) "&^%@#" == []

testToken :: IO ()
testToken = do
  assert $ parse integer " 0x1337  " == [(4918, "")]

testPythonList :: IO ()
testPythonList = do
  let parser = do
        token (string "[")
        n <- integer
        val <- many $ do
                token (string ",")
                n' <- integer
                return n'
        token (string "]")
        return (n : val)
  print $ parse parser "[ 1 , 2 , 3 , 4 ]"
  print $ parse parser "[1, 2, 3, 4]"

main :: IO ()
main = do
  testParsingOneChar
  testFunctor
  testApplicative
  testMonadCharChain
  testAlternative
  testCharPredicate
  testString
  testManyP
  testSomeP
  testPythonList
