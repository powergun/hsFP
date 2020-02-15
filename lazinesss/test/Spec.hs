import qualified FirstPrinciples.ForcedStrict
import qualified LazyPattern

main :: IO ()
main = do
  FirstPrinciples.ForcedStrict.demo
  LazyPattern.demo
