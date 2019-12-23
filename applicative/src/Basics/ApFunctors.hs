module Basics.ApFunctors
  ( demo
  )
where

import           Control.Applicative            ( (*>)
                                                , liftA2
                                                )

-- source: http://www.serpentine.com/blog/2008/02/06/the-basics-of-applicative-functors-put-to-practical-work/
-- *> combinator applies the parser on the left, throws away its result
-- then applies the parser on the right; you can think of > as "pointing at"
-- the parser whose value is really to be returned
demo :: IO ()
demo = do
  let a = pure (,) <*> [1, 2] <*> [3, 4]
      b = pure (,) <*> [1, 2] *> [3, 4]
      c = pure id <*> [1, 2] <* [3, 4]
  print a
  print b
  print c
  -- <$ applies the parser on the right: if it succeeds, it throws
  -- away its result and instead returns the value on the left;
  -- in other words, if we match "er", we return a space
  let e = pure id <*> "asd" <$ "er"
  print e
