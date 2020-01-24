
import qualified FirstPrinciples.Alternative
import qualified FirstPrinciples.Fractions
import qualified FirstPrinciples.INIParser
import qualified FirstPrinciples.Parser
import qualified TestINIParser

main :: IO ()
main = do
  -- FirstPrinciples.Parser.demo
  -- FirstPrinciples.Fractions.demo
  FirstPrinciples.Alternative.demo
  -- FirstPrinciples.INIParser.demo
  TestINIParser.spec

