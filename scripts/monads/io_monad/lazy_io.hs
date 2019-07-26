#!/usr/bin/env stack runghc

-- haskell design pattern P/84
-- this is explained in the IO section in the book, but I found
-- it helpful to see the application of monads
-- see the rest of the lazy io examples in data_munging/fileIO

demoIOActionsAsList :: IO ()
demoIOActionsAsList = do
  let ioActions = map putStrLn ["this", "won't", "run"]
  print "demo io actions as a list"
  -- performance the actions
  -- NOTE: !!! sequence_ is a Prelude function defined 
  -- in Data.Foldable!! 
  -- redefine here for demonstration only !!!
  sequence_ ioActions
  where
    -- recap >>
    -- class Monad m where
    --   (>>=)  :: m a -> (  a -> m b) -> m b
    --   (>>)   :: m a ->  m b         -> m b
    --   return ::   a                 -> m a
    --   fail   :: String -> m a
    -- The >> function is used when the function does not need 
    -- the value produced by the first/prev monadic operator.
    sequence_ :: [IO ()] -> IO ()
    sequence_ = foldr (>>) (return ())

main :: IO ()
main = do
  demoIOActionsAsList
