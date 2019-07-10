#!/usr/bin/env stack runghc

-- haskell design pattern P/60
-- ad hoc polymorphism

data Shape = Circle Float | Rect Float Float
area :: Shape -> Float
area (Circle r) = 3.1415 * r ^ 2
area (Rect length width) = length * width

-- alternation based ad hoc polymorphism
-- area function is dispatched over the alternations of the 
-- Shape type
demoAlternationAreaDispatched :: IO ()
demoAlternationAreaDispatched = do
    print "alternation based ad hoc polymorphism"
    print $ map area [Circle 1.0, Rect 1.0 2.0]

-- class based ad hoc polymorphism
data Circle_ = Circle_ Float
data Rect_ = Rect_ Float Float

class Shape_ a where
    area_ :: a -> Float

instance Shape_ Circle_ where
    area_ (Circle_ r) = 3.1415 * r ^ 2

instance Shape_ Rect_ where
    area_ (Rect_ length' width') = length' * width'

demoClassAreaDispatched :: IO ()
demoClassAreaDispatched = do
    print "class based ad hoc polymorphism"
    print $ area_ $ Circle_ 1.0
    print $ area_ $ Rect_ 1.0 2.0

main :: IO ()
main = do
    demoAlternationAreaDispatched
    demoClassAreaDispatched
