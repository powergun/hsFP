module MonoidOfFunctions (demo) where

import           Data.Map
import           Prelude  hiding (lookup)

config :: Map String String
config =
  let systemConf = fromList [("home", "/"), ("ncpu", "8")]
      userConf = fromList [("home", "/user"), ("ncpu", "4"), ("username", "doom")]
  -- note, keys in systemConf are ignored to respect the existing
  -- key-value of the same name
  in userConf <> systemConf

demo :: IO ()
demo = do
  print $ lookup "home" config
  print $ lookup "username" config
