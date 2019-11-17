module WrapEnv.ImplV1
  ( demo
  )
where

{-
note this usage in shellcheck Parser
also read:
why there is no record in haskell? how to introduce namespace?
https://gitlab.haskell.org/ghc/ghc/wikis/records

Mr.asks 
http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html
-}

import qualified Control.Monad.Reader          as Mr

data Environment = Environment {
  gameType :: String,
  mapName :: String
}

accessEnv :: Mr.ReaderT Environment IO ()
accessEnv = do
  mn <- Mr.asks mapName
  Mr.lift $ putStrLn $ "@@ map name: " ++ mn

demo :: IO ()
demo = do
  let env = Environment { gameType = "deathmatch", mapName = "e1m1" }
  Mr.runReaderT accessEnv env
