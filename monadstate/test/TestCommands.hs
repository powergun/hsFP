module TestCommands (runSpec) where

import Test.Hspec
import Commands.ImplV2

runSpec :: IO ()
runSpec = hspec $ describe "Test Commands" $ it
  "run a list of string-based commands, expect the result" $ do
    execute "n(t)(n)*nn(9)*" `shouldBe` "tttnnn|*|999|*|"
    execute "nnnnnn" `shouldBe` ""
    execute "(o)*nnn" `shouldBe` "ooo|*|"
