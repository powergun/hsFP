module BinTree.Applicative
  ( demo
  )
where

-- haskell cookbook L2555
-- L2645
-- Applicative allows sequencing, without needing to know about
-- intermediate results. Hence an Applicative is stronger than 
-- a Functor (but weaker than a monad)

data Tree a = Leaf a
            | Node (Tree a) a (Tree a)
            deriving (Show, Eq)

treeSUT :: Tree String
treeSUT = Node
  (Node (Leaf "/dv/man")
        "/dv"
        (Node (Leaf "/dv/usr/d1") "/dv/usr" (Leaf "/dv/usr/c1"))
  )
  "/"
  (Node (Node (Leaf "/pd/dat/rig") "/pd/dat" (Leaf "/pd/dat/mdl"))
        "/pd"
        (Leaf "/pd/man")
  )

singleton :: Tree String
singleton = Node (Leaf "x") "." (Leaf "y")

instance Functor Tree where
  fmap f (Leaf x    ) = Leaf (f x)
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

instance Applicative Tree where
  -- this won't work: pure id will stop at the Leaf level instead 
  -- of recursing down, which fails the first law
  -- pure x = Leaf x
  -- haskell cookbook L2627
  -- to be able to satisfy this law we have to make the "pure x" 
  -- instance of a binary tree an infinite recursively defined
  -- tree
  pure x = let t = Node t x t in t
-- MY NOTES:
-- because left-f applies (only) to the left branches and 
-- right-f applies (only) to the right branches, I can code a
-- natural branching logic 
  (Leaf f      ) <*> (Leaf x    ) = Leaf (f x)
  (Node _ f _  ) <*> (Leaf x    ) = Leaf (f x)
  (Leaf f      ) <*> (Node l x r) = Leaf (f x)
  (Node lf f rf) <*> (Node l x r) = Node (lf <*> l) (f x) (rf <*> r)

demo_ :: IO ()
demo_ = do
  print "//////// demo <*>"
  print $ (\s t -> [(head s)] ++ [(last t)]) <$> singleton <*> treeSUT
  print $ (\s t -> [(head s)] ++ [(last t)]) <$> treeSUT <*> singleton

-- haskell cookbook L2609
-- an application of identity should not change the data
-- (pure id <*> v) == v
proveIdentityLaw :: IO ()
proveIdentityLaw = do
  print "//////// prove Identity Law is met /////////////////////"
  print $ (pure id <*> treeSUT) == treeSUT

-- Homomorphism can be proved by the definition of <*>
proveHomomorphism :: IO ()
proveHomomorphism = print 1

-- f (a -> b) -> f a == f b, which should be equivalent to
-- f ((a -> b) -> b) -> f (a -> b) == f b
proveInterchangeable :: IO ()
proveInterchangeable = do
  -- Prelude> a = ($ "$$")
  -- Prelude> :t a
  -- a :: ([Char] -> b) -> b
  -- Prelude> a (\_ -> 1)
  -- 1
  print "//////// prove Interchangeable /////////////////////////"
  let f s = s ++ "_called"
      treef = pure (++) <*> treeSUT
  -- ($ "$$") "absorbs" the f encapsulated by treef and calls it
  -- with its hardcoded value (it provides the right operand, 
  -- whereas the f in treef provides the left operand)
  -- MY NOTES:
  -- I can verify this in ghci
  -- f = ($ "asd")
  -- Prelude> :t f
  -- f :: ([Char] -> b) -> b 
  -- f consumes a function 
  -- therefore pure ($ "$$") consumes a function
  print $ (treef <*> pure "$$") == (pure ($ "$$") <*> treef)

proveComposition :: IO ()
proveComposition = do
  print "//////// prove Composition ///////////////////////////////"
  let treeu = pure (\s t -> s ++ "_u_" ++ t) <*> treeSUT
      treev = pure (\s t -> s ++ "_v_" ++ t) <*> treeSUT
      treew = pure (\s t -> s ++ "_w_" ++ t) <*> treeSUT
  -- showing how does composition work with pure functions
  -- "which function gets called first ?"
  print $ (((+ 1) . (* 3)) 3) == ((+ 1) ((* 3) 3))
  print
    $  (pure (.) <*> treeu <*> treev <*> treeSUT)
    == (treeu <*> (treev <*> treeSUT))

-- these laws give a way to cross-check whether our impl is correct
-- these laws allow an Applicative to embed a computation and 
-- move it freely

demo :: IO ()
demo = do
  demo_
  proveIdentityLaw
  proveInterchangeable
  proveComposition
