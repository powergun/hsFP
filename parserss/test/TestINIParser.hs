{-# LANGUAGE OverloadedStrings #-}

module TestINIParser (spec) where

import           Data.ByteString
import           FirstPrinciples.INIParser
import           Test.Hspec
import           Text.Trifecta

toMaybe :: (Result a) -> Maybe a
toMaybe (Success a) = Just a
toMaybe _           = Nothing

spec = hspec $ do
  describe "Test INI-Parser" $ do
    it "Parse assignments" $ do
      let sut = "map=e1m1" :: ByteString
          o = toMaybe $ parseByteString parseAssignment mempty sut
      o `shouldBe` (Just ("map", "e1m1"))
