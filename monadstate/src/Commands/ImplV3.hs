module Commands.ImplV3
  ( execute
  )
where
import           Control.Monad                  ( mapM_ )
import qualified Control.Monad.State           as Ms
import           Text.Parsec
import           Text.Parsec.String
execute :: String -> String
execute cmdStr = Ms.evalState (runCommands cmdStr) (ExeState { output = "" })
runCommands :: String -> Ms.State ExeState String
runCommands cmdStr = do
  let (Right cmds) = parse parseCommands "there" cmdStr
  mapM_ runCommand cmds
  Ms.gets output
newtype ExeState = ExeState { output :: String }
data Command = Noop
             | Tripplet Char
             | Marker
runCommand :: Command -> Ms.State ExeState ()
runCommand Noop = return ()
runCommand (Tripplet elem) =
  Ms.modify $ \es -> es { output = output es ++ [elem, elem, elem] }
runCommand Marker = Ms.modify $ \es -> es { output = output es ++ "|*|" }
parseCommands :: Parser [Command]
parseCommands = many (parseNoop <|> parseTripplet <|> parseMarker)
parseNoop :: Parser Command
parseNoop = char 'n' >> return Noop
parseMarker :: Parser Command
parseMarker = char '*' >> return Marker
parseTripplet :: Parser Command
parseTripplet = do
  char '('
  elem <- noneOf "()"
  char ')'
  return (Tripplet elem)


