module CommandMonadV1 (execute) where

import           Control.Monad.State as Ms
import           Data.List           (intercalate)
import           Text.Parsec
import           Text.Parsec.String  (Parser)

data ExeState = ExeState { position :: Int
                         , haystack :: String
                         , output   :: [String] }

createState s = ExeState { position = 0, haystack = s, output = [] :: [String] }

data Command = Noop
             | Tripplet Char
             | Marker
             deriving (Eq, Show)

parseCommands :: String -> [Command]
parseCommands cmdStr =
  let doParse = many (parseNoop <|> parseTripplet <|> parseMarker)
  in case parse doParse [] cmdStr of
        Right cmds -> cmds
        Left  err  -> error (show err)

parseNoop :: Parser Command
parseNoop = do
  char 'n'
  return Noop

parseTripplet :: Parser Command
parseTripplet = do
  char '('
  elem <- noneOf ")"
  char ')'
  return (Tripplet elem)

parseMarker :: Parser Command
parseMarker = do
  char '*'
  return Marker

runCommands :: [Command] -> Ms.State ExeState [String]
runCommands cmds = do
  mapM_ runCommand cmds
  Ms.gets output

runCommand :: Command -> Ms.State ExeState ()
runCommand Noop = return ()
runCommand Marker =
  Ms.modify $ \es -> es { output = output es ++ ["|*|"] }
runCommand (Tripplet elem) =
  Ms.modify $ \es -> es { output = output es ++ [[elem, elem, elem]] }

execute :: String -> String
execute cmdStr =
  let cmds = parseCommands cmdStr
      o = Ms.evalState (runCommands cmds) (createState "")
      oStr = intercalate "" o
  in oStr
