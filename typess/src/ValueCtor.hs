module ValueCtor
    ( Shape(..)
    )
where

-- test this in ghci
-- λ> :t Circle
-- Circle :: Float -> Float -> Float -> Shape
-- λ> :t Rectangle
-- Rectangle :: Float -> Float -> Float -> Float -> Shape
data Shape = Circle Float Float Float
           | Rectangle Float Float Float Float
