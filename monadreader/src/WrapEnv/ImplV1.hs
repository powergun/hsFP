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

-- reader monad

useEnv :: Mr.Reader Environment String
useEnv = do
  env <- Mr.ask
  return $ "||||" ++ (gameType env) 

demoReader :: IO ()
demoReader = do
  let env = Environment { gameType = "demolition", mapName = "grid" }

  -- `Mr.runReader useEnv env` returns String 
  -- in contrary `Mr.runReaderT accessEnv env` returns IO ()
  print $ Mr.runReader useEnv env

-- this is not reader monad but reader monad transformer

accessEnv :: Mr.ReaderT Environment IO ()
accessEnv = do
  mn <- Mr.asks mapName
  Mr.lift $ putStrLn $ "@@ map name: " ++ mn

demoReaderTransformer :: IO ()
demoReaderTransformer = do
  let env = Environment { gameType = "deathmatch", mapName = "nuketown" }
  Mr.runReaderT accessEnv env

demo :: IO ()
demo = do
  demoReader
  demoReaderTransformer