module FirstPrinciples.Exercises (demo) where

import           Control.Monad.Identity
import           Control.Monad.Trans        (lift)
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State

-- rDec is a function that should get its argument in the context
-- of Reader and return a value decremented by one
rDec :: Num a => Reader a a
rDec = ask >>= return . (+ (-1))

-- rShow is show, but in Reader.
rShow :: Show a => ReaderT a Identity String
rShow = ask >>= return . show

-- rPrintAndInc will first print the input with a greeting, then return
-- the input incremented by one.
rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = do
  env <- ask
  lift (putStr "hi: ")
  lift (putStr . show $ env)
  lift (putStrLn "")
  return $ env + 1

-- sPrintIncAccum first prints the input with a greeting, then puts the
-- incremented input as the new state, and returns the original input as
-- a String.
sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = do
  st <- get
  lift (putStr "Hi: ")
  lift (putStr . show $ st)
  lift (putStrLn "")
  put . (+ 1) $ st
  return . show $ st

demo :: IO ()
demo = do
  print "//// Chapter Exercises"
  print $ runReader rDec 1
  print $ fmap (runReader rDec) [1..10]
  print $ runReader rShow 1
  print $ fmap (runReader rShow) [1..10]
  print =<< runReaderT rPrintAndInc 1
  print =<< traverse (runReaderT rPrintAndInc) [1..10]
  print =<< runStateT sPrintIncAccum 10
  print =<< mapM (runStateT sPrintIncAccum) [1..5]
