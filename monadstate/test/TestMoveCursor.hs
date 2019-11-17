-- haskell cookbook L3221
-- UPDATE:
-- compare this move-cursor example with monadreader/MoveCursor
-- using State simplifies the processing logic

module TestMoveCursor
  ( runSpec
  )
where

import           Test.Hspec
import           MoveCursor.ImplV1
import qualified Control.Monad.State           as S

runSpec :: IO ()
runSpec =
  hspec
    $ describe "Test MoveCursor"
    $ it "move cursor using a list of instructions, expect final position"
    $ do
        let moves = [South 100, East 100, North 100, West 100]
            c1    = S.execState (applyMoves moves) (Cursor 0 0)
            c2    = S.execState (applyMoves moves) (Cursor 12 12)
        Cursor 0 0 `shouldBe` c1
        Cursor 12 12 `shouldBe` c2
