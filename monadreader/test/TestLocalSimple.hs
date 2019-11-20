
module TestLocalSimple (demo) where

import qualified Local.Simple

demo :: IO ()
demo = do
    txts <- Local.Simple.triplet "iddqd"
    print txts
