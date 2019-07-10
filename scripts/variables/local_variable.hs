#!/usr/bin/env stack runghc

-- real world haskell P/101
-- let expression: introduce local var in the function body

demoLetScope =
  -- can use {} with ; or rely on indentation
  -- can not miss "in"
  -- form 1
  -- let { a = 10
  --   ; b = 20
  --   ; c = 30
  -- } 
  -- form 2
  let a = 10
      b = 20
      c = 30
  in [a, b, c]

-- P/102 
-- we can "nest" multiple let blocks inside each other in an
-- expression
demoNestedLetScope =
  let a = 10
  in let b = 20
     in a + b

-- whitespace is significant in where clauses
demoWhereClause = iddqd
  where iddqd = 100

main :: IO ()
main = do
  print demoLetScope
  print demoNestedLetScope
  print demoWhereClause
