module FirstPrinciples.Parser
  ( demo
  )
where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop
oneTwo = char '1' >> char '2'
oneTwo' = oneTwo >> stop
oneTwoOnly = oneTwo >> eof

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

demo :: IO ()
demo = do
  putStrLn "\n"
  testParse one'
  putStrLn "\n"
  testParse oneTwo'
  putStrLn "\n"
  print $ parseString oneTwoOnly mempty "123"

