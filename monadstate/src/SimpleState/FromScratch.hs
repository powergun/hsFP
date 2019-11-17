module SimpleState.FromScratch
  ( demo
  )
where

-- See: state_monad_diagram.png

-- programming haskell P168
-- the state monad
-- MY NOTES:
-- I like the book's approach in introducing Monad: Monad is a
-- generic concept that shares the characteristics of Functor and 
-- Applicative. Therefore a Monad can also be used inplace of 
-- the latters. The new GHC release enforces this so that to 
-- implement Monad typeclass, one has to also implement the Functor 
-- Applicative typeclass. The examples below demonstrate that the 
-- StateTransformer Monad can also work as <$> and <*>

type State = Int

-- Given that StateTransformer is a parameterised type, it is 
-- natural to try and make it into a monad so that the do notion 
-- can then be used to write stateful programs.
-- ... redefine StateTransformer using the newtype mechanism
-- which requires introducing a dummy ctor
newtype StateTransformer a = StateTransformer (State -> (a, State))

-- it is also convenient to define a special purpose application 
-- function for this type, which simply removes the dummy ctor
app :: StateTransformer a -> State -> (a, State)
app (StateTransformer st) x =
  --                 ^^^ StateTransformer encapsulates a function
  st x

-- programming haskell P169
-- the "let" mechanism is similar to "where" mechanism, except
-- that it allows local definition to be made at the level of
-- expressions rather than at the level of function definitions
-- MY NOTES:
-- I can not use "where" statement here because "s" is the argument
-- where scope does not see it as a legit variable
-- Also note that I can use "fst" to only retrieve the first elem
-- of the tuple returned by "app", but the code is not as short
instance Functor StateTransformer where
  fmap g st =
    StateTransformer $ \s -> let (x, s') = app st s -- remove dummy ctor; unwrap
                                                    in (g x, s')
      -- in the relabelling tree example, the return type here 
      -- is ((Leaf x), s') or a recursion

-- programming haskell P170 (great diagram in the book!!)
-- <*>: applies a StateTransformer that returns a function to a 
--   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- StateTransformer that returns an argument to give a StateTransformer
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- that returns the result of applying the function to the argument
-- ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- MY NOTES:
-- see the use of app function to unwrap
instance Applicative StateTransformer where
  pure x = StateTransformer (\s -> (x, s))
-- MY NOTE:
-- DO not mix up applicative and monad
-- function f comes at different position
-- f (a -> b) -> f a -> f b
  stf <*> stx = StateTransformer $ \s ->
    let (f, s' ) = app stf s
        (x, s'') = app stx s'
    in  (f x, s'')

-- programming haskell P170
-- st >>= f applies the state transformer st to an initial state 
-- s, then applies the function f to the resulting value x to 
-- give a new state transformer f x, which is then applied to 
-- the new state s' to give the final result
-- Note that within the definition for >>= we produce a new state 
-- transformer f x whose behavior may depend on the result value
-- of the first argument x; whereas with <*> we are restricted to 
-- using state transformers that are explicitly supplied as 
-- arguments. As such >>= provides extra flexibility
--                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
-- TODO: demonstrate this flexibility
instance Monad StateTransformer where
  -- MY NOTE: do not mix monad with applicative
  --          f comes at different position !!
  -- (>>=)  :: m a -> (a -> m b) -> m b
  -- ST a -> (a -> ST b) -> ST b
  st >>= f =
  -- MY NOTES:
  -- recall that the function StateTransformer wraps is of type
  -- S -> (a, S); therefore the lambda definition here must
  -- use app to yield to result as (some-value, s')
    StateTransformer $ \s -> let (x, s') = app st s in app (f x) s'

demoSTAsFunctor :: IO ()
demoSTAsFunctor = do
  let stA = (StateTransformer (\s -> (11, s)) :: StateTransformer Int)
      f   = \_ -> 15
      stB = fmap f stA
  -- replace encapsulated value 11 with 15
  print $ app stA 0
  print $ app stB 0

demoSTAsApplicative :: IO ()
demoSTAsApplicative = do
  let st1 = pure "iddqd" :: StateTransformer String
      st2 = pure "idkfa" :: StateTransformer String
      f   = \str1 str2 -> str1 ++ "_called_" ++ str2
  print $ app (f <$> st1 <*> st2) 0

demoST :: IO ()
demoST = do
  let st1 = pure "iddqd" :: StateTransformer String
      useBind =
        st1
          >>= (\s -> return $ s ++ "_called")
          >>= (\s -> return $ "received_" ++ s)
      -- A gentle introduction to Monad - Monadic class
      -- MY NOTES
      -- note how do syntax saves the use of lambda and the anonymous
      -- argument s
      useDo = do
        s <- st1
        s <- return $ s ++ "^called"
        return $ "received^" ++ s
  print $ app useBind 0
  print $ app useDo 0

demo :: IO ()
demo = do
  demoSTAsFunctor
  demoSTAsApplicative
  demoST
