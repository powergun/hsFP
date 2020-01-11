module FirstPrinciples.Parser
  ( demo
  )
where

import           Text.Trifecta

stop :: Parser a
stop = unexpected "stop"

one = char '1'
one' = one >> stop

demo :: IO ()
demo = return ()
