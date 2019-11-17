module Commands.ImplV2 (execute) where
  import           Control.Monad       (mapM_)
  import qualified Control.Monad.State as Ms
  import           Data.List           (intercalate)
  import           Text.Parsec         (char, many, noneOf, parse, (<|>))
  import           Text.Parsec.String  (Parser)
  execute :: String -> String
  execute cmdStr =
    let (Right cmds) = parse parseCommands [] cmdStr
        o = Ms.evalState (runCommands cmds) (ExeState { output = [] :: [String] })
        oStr = intercalate "" o
    in oStr
  data Command = Noop
               | Tripplet Char
               | Marker
  newtype ExeState = ExeState { output :: [String] }
  parseNoop :: Parser Command
  parseNoop = do
    char 'n'
    return Noop
  parseTripplet :: Parser Command
  parseTripplet = do
    char '('
    elem <- noneOf "()"
    char ')'
    return (Tripplet elem)
  parseMaker :: Parser Command
  parseMaker = do
    char '*'
    return Marker
  parseCommands :: Parser [Command]
  parseCommands =
    many (parseNoop <|> parseMaker <|> parseTripplet)
  runCommand :: Command -> Ms.State ExeState ()
  runCommand Noop = return ()
  runCommand (Tripplet elem) =
    Ms.modify $ \es -> es { output = output es ++ [[elem, elem, elem]] }
  runCommand Marker =
    Ms.modify $ \es -> es { output = output es ++ ["|*|"] }
  runCommands :: [Command] -> Ms.State ExeState [String]
  runCommands cmds = do
    mapM_ runCommand cmds
    Ms.gets output
  