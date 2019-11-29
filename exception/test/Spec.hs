import qualified EitherTry

import qualified TwoFilesProblem

main :: IO ()
main = do
  EitherTry.demo
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file1")
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file12")
