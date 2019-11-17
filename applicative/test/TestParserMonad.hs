module TestParserMonad
  ( runSpec
  )
where

import           Test.Hspec
import           Control.Monad
import           Data.Char

import           ParserMonad.ImplV1

runSpec :: IO ()
runSpec = hspec $ describe "Test ParserMonad" $ do

  it "parse one char" $ do
    parse (char 't') "there" `shouldBe` [('t', "here")]
    parse (char 't') "iddqd" `shouldBe` []

  it "basic functor parser syntax" $ do
    parse (charUpper 't') "there" `shouldBe` [('T', "here")]
    parse (toLower <$> char 'I') "ID" `shouldBe` [('i', "D")]

  it "basic applicative parser syntax" $ do
    parse (pure 'X') "there" `shouldBe` [('X', "there")]
    parse (pure toUpper <*> char 'x') "xxd" `shouldBe` [('X', "xd")]
    parse (string4 "id") "iddqd" `shouldBe` [("", "iddqd")]
    parse (string4 "idfa") "idfa-iddqd" `shouldBe` [("idfa", "-iddqd")]
    parse (string4 "1337") "ad" `shouldBe` []

  it "use chain-of-parsers" $ do
    let cheatcode = do
          char 'i'
          char 'd'
          char 'd'
          return "invincible"
    parse cheatcode "iddqd" `shouldBe` [("invincible", "qd")]

  it "use alternative parser (<|>) - the basis for parser-combinator" $ do
    let cheatcodes = do
          char 'i'
          char 'd'
          char 'd' <|> char 'k'
          return "cheats"
    parse cheatcodes "iddqd" `shouldBe` [("cheats", "qd")]
    parse cheatcodes "idkfa" `shouldBe` [("cheats", "fa")]

  it "use char-predicate" $ do
    parse (sat isDigit) "0x1" `shouldBe` [('0', "x1")]
    parse (sat isDigit) "abc" `shouldBe` []

  it "combine parsers" $ do
    -- let parsers = mapM ["iddaf", "iddqd", "idkfa", "idclip"] $ \s -> do
    --                 string "idd" <|> string "idk" <|> string "idc"
    --                 return "1"
    let parsers =
          (string "idd" <|> string "idk" <|> string "idc") >> return "1"
    parse parsers "idfa" `shouldBe` []
    parse parsers "iddqd" `shouldBe` [("1", "qd")]

  it "parse alpha-numeric vals, using some(): expect at least 1 val" $ do
    parse (some $ sat isDigit) "1234abc" `shouldBe` [("1234", "abc")]
    parse (some $ sat isAlphaNum) "&^%@#" `shouldBe` []

  it "parse alpha-numeric vals, using many(): allow no val" $ do
    parse (many $ sat isDigit) "1234abc" `shouldBe` [("1234", "abc")]
    parse (many $ sat isAlphaNum) "&^%@#" `shouldBe` [("", "&^%@#")]

  it "parse hexdecimal values" $ do
    let v = parse integer " 0x1337  "
    v `shouldBe` [(4919, "")]

  it "parse a Python-like list" $ do
    let parser = do
          token (string "[")
          n   <- integer
          val <- many $ do
            token (string ",")
            -- NOTE: simplified
            -- n' <- integer
            -- return n'
            integer
          token (string "]")
          return (n : val)
    parse parser "[ 1 , 2 , 3 , 4 ]" `shouldBe` [([1, 2, 3, 4], "")]
    parse parser "[1, 2, 3, 4]" `shouldBe` [([1, 2, 3, 4], "")]
