import qualified EitherTry
import qualified TwoFilesProblem

import           BestPractices.ReadException

demoReadException :: IO ()
demoReadException = do
  print (readM "hello" :: Either SomeException Int)
  print (readM "5" :: Either SomeException Int)
  print (readM "5" :: Either SomeException Bool)

  -- Also works in plain IO
  res1 <- readM "6"
  print (res1 :: Int)
  res2 <- readM "not an int"
  print (res2 :: Int) -- will never get called (exit 1)

main :: IO ()
main = do
  EitherTry.demo
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file1")
  TwoFilesProblem.demo ("./testdata/file1", "./testdata/file12")

  demoReadException
