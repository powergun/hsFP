module WrapInteger.ImplV1
  ( demo
  , demo2
  )
where

import           Control.Monad.Reader
import qualified Control.Monad.Reader as Mr

-- haskell cookbook L3463
-- reader monad is a monad transformer with the purpose of
-- providing an environment
-- ReaderT :: r m a, defined in Control.Monad.Reader module
-- in the mtl library, usually each transformer has an associated
-- type class.
-- The transformer ReaderT is an instance of MonadReader type class

-- MonadReader provides three functions:
-- ask function gets the current environment
-- can be used to use a function that takes the current environment
-- and produces some value that can be used in the context of the
-- monad

-- function local takes a function that produces another environment
-- the supplied computation is executed under the modified
-- environment, however the current environment is unaffected
example :: ReaderT Int IO ()
example = do
  s <- ask
  lift $ putStrLn $ "current env state is " ++ (show s)
  s_is_10 <- asks (== 10)
  lift $ putStrLn $ "current state is 10? " ++ (show s_is_10)

cover :: ReaderT Int IO ()
cover = do
  example
  local (const 10) example

demo :: IO ()
demo = do
  runReaderT cover 100

-- a cleaner example:

compute :: Mr.ReaderT [Int] IO [Int]
compute = do
  nums <- Mr.ask
  Mr.lift $ putStrLn $ "+ numbers: " ++ show nums
  return [1, 2, 3]

demo2 :: IO ()
demo2 = do
  Mr.runReaderT compute [10, 20, 30]
  return ()
