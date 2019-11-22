module Transactions.WriterImpl (demo) where

-- ImplVxx uses writer monad transformer;
-- this example uses writer monad

import qualified Control.Monad.Writer as Mw

compute1 :: Mw.Writer [(String, Int)] ()
compute1 = do
  Mw.tell [("there is a cow", 1337)]

compute2 :: Int -> Mw.Writer [(String, Int)] ()
compute2 param = do
  Mw.tell [("/assets/textures", param)]

compute :: Int -> Mw.Writer [(String, Int)] ()
compute param = do
  compute1 
  compute2 param

demo :: IO ()
demo = do
  let (ret, acc) = Mw.runWriter $ compute 84213
  print acc
