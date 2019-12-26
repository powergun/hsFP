module TestValidation (demo) where

import           Test.Hspec
import           Validation.Newtype

demo :: IO ()
demo = hspec $ do
  let badForm = ReservationJson "-" 123.1 "doom guy" "iddqd@doom.org"

  describe "Applicative Validation" $ do
    it "Given bad form, expect rejected with errors" $ do
      let r = validate badForm
      hasError r "Invalid date" `shouldBe` True
      hasError r "Not a positive integer" `shouldBe` True

  describe "Applicative Validation (only validation, not converting)" $ do
    it "Given bad form, expect rejected with errors" $ do
      let r = validateJson badForm
      hasError r "Invalid date" `shouldBe` True
      hasError r "Not a positive integer" `shouldBe` True
