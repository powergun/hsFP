#!/usr/bin/env stack runghc

-- real world haskell P/360
-- in abstract algebra, there is a simple abstract structure 
-- called a monoid
-- in order to be considered a monoid, an object must have two 
-- properties:
-- an associative binary op a * (b * c)
-- an identity value, a * e == a and e * a == a
-- the rules for monoid don't say what the binary operator must 
-- do, merely that such an operator must exist

