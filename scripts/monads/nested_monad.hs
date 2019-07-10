#!/usr/bin/env stack runghc

-- haskell cookbook L3156

-- Maybe monad is nested inside IO monad
foo :: IO (Maybe String)
foo = do 
  return (Just "foo")

bar :: IO (Maybe String)
bar = do 
  return Nothing

-- L3169
-- we need to switch context from IO monad to the Maybe monad
-- to be able to do the computation in the monad that we are 
-- interested in
doConcat :: IO (Maybe String) -> IO (Maybe String) -> IO (Maybe String)
doConcat lhs rhs = do -- we are in IO monad
  x <- lhs -- get Maybe Int from xbar
  y <- rhs
  return $ do -- we are in Maybe moand
    xStr <- x -- get data stored in x
    yStr <- y
    return (xStr ++ " " ++ yStr) -- return Nothing if either x 
                                 -- or y is Nothing

demoConcat :: IO ()
demoConcat = do
  doConcat foo foo >>= print
  doConcat foo bar >>= print

main :: IO ()
main = do
  demoConcat