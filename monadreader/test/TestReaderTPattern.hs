module TestReaderTPattern (demo) where

import qualified ReaderTPattern.Demo as Patterns

demo :: IO ()
demo = do
  print =<< Patterns.getConfigFromFile "./testdata/good.conf"
  print =<< Patterns.getConfigFromFile "./testdata/bad.conf"
  print =<< Patterns.getConfigFromFile "./testdata/noexist.conf"
