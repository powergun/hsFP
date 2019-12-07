{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module FromScratch.Monad (demo) where

import qualified Control.Monad.Reader as R
import qualified Control.Monad.State  as S
import qualified Control.Monad.Trans  as Mt

type Environment = [String]
type State = [Int]

newtype M a = M {
  runM :: Environment -> State -> (a, State)
}

-- my so-called "simple approach"
type MEasy a = R.ReaderT Environment (S.StateT State IO) a

-- I had to review monadstate/ImplV4.hs to remember the implementation
-- details
-- I also found that I can leave runM' = undefined to quickly
-- sketch out the functor, applicative and monad instances.
instance Functor M where
  fmap f m =
    let runM' env st = let (a, st') = runM m env st
                       in (f a, st')
    in M runM'

instance Applicative M where
  pure a =
    let runM' env st = (a, st)
    in M runM'
  mf <*> ma =
    let runM' env st = let (f, st') = runM mf env st
                           (a, st'') = runM ma env st'
                       in (f a, st'')
    in M runM'

instance Monad M where
  return = pure
  ma >>= f =
    let runM' env st = let (a, st') = runM ma env st
                           mb = f a
                           (a', st'') = runM mb env st'
                       in (a', st'')
    in M runM'

getState :: M State
getState =
  let runM' env st = (st, st)
  in M runM'

setState :: State -> M State
setState st' =
  let runM' env st = (st', st')
  in M runM'

getEnv :: M Environment
getEnv =
  let runM' env st = (env, st)
  in M runM'

-- setEnv won't work!!
-- Environment is not returned to the rhs of the bind operator
-- there is no way to update it!!
-- setEnv :: Environment -> M Environment

compute :: M Int
compute = do
  st <- getState
  env <- getEnv
  setState $ [length env, length st]
  return 12

computeT :: MEasy Int
computeT = do
  st <- R.lift $ S.get
  env <- R.ask
  R.lift $ S.put $ [length env, length st]
  -- can perform IO here
  Mt.liftIO $ print "there is acow"
  return 12

demo :: IO ()
demo = do
  let ret = runM compute ["iddqd"] [1 .. 4]
  print ret
  ret' <- S.runStateT (R.runReaderT computeT ["iddqd"]) [1 .. 4]
  print ret'
