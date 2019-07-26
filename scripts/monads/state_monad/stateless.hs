#!/usr/bin/env stack runghc

import Control.Monad (ap, liftM)

-- degenerated case where State Monad does not book
-- keep a state at all
-- note that all the state manipulation logic (app() function
-- and >>= still work like the stateful version)

data ST a = ST (a) -- a is a function, its has no argument and 
                   -- returns a predetermined value

app :: ST a -> a
app (ST st) = st -- pattern matching works because the data ctor
                 -- exists

-- this works, following the stateful version, 
-- this follows functor -> applicative -> monad path

-- instance Functor ST where
--   fmap f sta =
--     let x = app sta
--     in ST (f x)

-- instance Applicative ST where
--   pure x = ST (x)
--   stf <*> sta =
--     let f = app stf
--         x = app sta
--     in ST (f x)

-- instance Monad ST where
--   return = pure
--   st >>= f =
--     let x = app st
--     in ST (app (f x))

-- using the "shortcuts" (ap, liftM)
instance Monad ST where
  return x = ST (x)
  st >>= f =
    let x = app st
    in ST (app (f x))
instance Applicative ST where
  pure = return
  (<*>) = ap
instance Functor ST where
  fmap = liftM

demoStateless :: String -> IO ()
demoStateless s = do
  let st1 = return s :: ST String
      st2 = do
        s <- st1
        s <- return $ case (length s < 5) of
          True -> s ++ "_I"
          False -> s
        s <- return $ case (length s < 7) of
          True -> s ++ "_II"
          False -> s
        return s
  print $ app st2
  -- ST is essentially interchangeable with functor
  -- there is no benefit of using State Transformer the Monad
  print $ app $ fmap (\s -> s ++ "_functor") st1

main :: IO ()
main = do
  demoStateless "IDD"
  demoStateless "IDNOCLIP"
