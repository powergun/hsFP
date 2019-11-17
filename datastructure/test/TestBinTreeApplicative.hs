module TestBinTreeApplicative
  ( runSpec
  )
where

import qualified BinTree.Applicative

runSpec :: IO ()
runSpec = BinTree.Applicative.demo
