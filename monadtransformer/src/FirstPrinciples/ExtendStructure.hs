module FirstPrinciples.ExtendStructure (demo) where

import           Control.Monad.Identity     (Identity (..))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import           Control.Monad.Trans.Maybe  (MaybeT (..), runMaybeT)
import qualified Data.Map                   as DM
import           Data.Maybe                 (maybe)

param' :: String -> MaybeT Identity Int
param' = MaybeT . Identity . (flip DM.lookup envs)

param'' :: String -> ExceptT String Identity Int
param'' k = ExceptT
          . Identity
          . (maybe (Left $ "missing key: " ++ k) Right)
          $ (DM.lookup k envs)

type Struct = (Int, Int, Int, Int)

envs :: DM.Map String Int
envs = DM.fromList [ ("e1m1", 2)
                   , ("e1m2", 3)
                   , ("e1m3", 4)
                   , ("e1m4", 21)
                   ]

demo :: IO ()
demo = do
  print "//// Temporarily extend structure"
  demoMaybeT
  demoExceptT

demoMaybeT :: IO ()
demoMaybeT = do
  let o = runMaybeT $ do
            a <- param' "e1m1"
            b <- param' "e1m2"
            c <- param' "e1m3"
            d <- param' "e1m4"
            return ((a, b, c, d) :: Struct)
  print o

  let o' = runMaybeT $ do
             a <- param' "e1m1"
             b <- param' "e1m2"
             c <- param' "e3m3"
             d <- param' "e1m4"
             return ((a, b, c, d) :: Struct)
  print o'

demoExceptT :: IO ()
demoExceptT = do
  let o = runExceptT $ do
            a <- param'' "e1m1"
            b <- param'' "e1m2"
            c <- param'' "e1m3"
            d <- param'' "e1m4"
            return ((a, b, c, d) :: Struct)
  print o

  let o' = runExceptT $ do
            a <- param'' "e1m1"
            b <- param'' "e1m2"
            c <- param'' "e3m3"
            d <- param'' "e1m4"
            return ((a, b, c, d) :: Struct)
  print o'
