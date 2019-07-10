#!/usr/bin/env stack runghc

-- real world haskell P/105
-- after the let or where keyword, 
-- > if the line that follows is empty or its indentation is 
--   further to the right, it is considered as a continuation 
--   of the previous line 
-- > if the indentation is the same as the start of the preceding 
--   item, it is treated as beginning of a new item in the same block

-- observe x, y and in print
-- observe 5 7 are treated as arguments to makeTuple, whereas
-- y = 6 is a separate definition (see also the example in P/105)

makeTuple x y = (x, y)

demoLetIndentation =
  -- |introduce x, y bindings (variables) and use them later in the
  -- | expression
  -- | the scope of these bindings is within the print() expression
  -- | therefore they are not available outside this scope
  let x = makeTuple 5
        7
      y = 6
  in print (show x ++ show y)
  -- use brace and oneliner style
  -- let { x = 10; y = 123 } in print (x + y)

main :: IO ()
main = do
  demoLetIndentation
  print 1