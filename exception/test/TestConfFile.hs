module TestConfFile (demo) where

import qualified ConfFile.Demo     as Patterns
import qualified ConfFile.SafeRead as SafeRead

demo :: IO ()
demo = do
  Patterns.runProgram "./testdata/good.conf"
  Patterns.runProgram "./testdata/good_space.conf"
  Patterns.runProgram "./testdata/non-exist41312"
  Patterns.runProgram "./testdata/bad.conf"

  SafeRead.demo
