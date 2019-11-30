import qualified EitherTry
import qualified TwoFilesProblem
import qualified ThrowError

main :: IO ()
main = do
  EitherTry.demo
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file1")
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file12")
  ThrowError.demo
