module FirstPrinciples.RWSTMonad
  ( demo
  )
where

import           Control.Monad.Trans.RWS.Lazy

type App = RWS () () [String] Int

compute :: App
compute = do
  st <- get
  put (st ++ [".end"])
  return 1

demo :: IO ()
demo = do
  print "//// RWST rediscovered"
  print $ runRWS compute () ["iddqd", "idkfa"]
