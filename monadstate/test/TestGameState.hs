
module TestGameState
  ( runSpec
  )
where

import           Test.Hspec
import qualified GameState.ImplV1              as Sut

runSpec :: IO ()
runSpec = hspec $ describe "Test GameState (a minimal State example)" $ do

  it "play a game, expect final game-state and score" $ do
    let (st, sc) = Sut.play "abcaaacbbcabbab"
    st `shouldBe` True
    sc `shouldBe` 2

  it "empty input, expect initial game-state and 0-score" $ do
    let (st, sc) = Sut.play ""
    st `shouldBe` False
    sc `shouldBe` 0
