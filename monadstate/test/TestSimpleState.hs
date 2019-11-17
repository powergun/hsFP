module TestSimpleState
  ( runSpec
  )
where

-- source
-- haskell cookbook
import           Test.Hspec
import qualified SimpleState.ImplV4            as SSI

import qualified SimpleState.FromScratch
import qualified SimpleState.Stateless
import qualified SimpleState.Resource

runSpecTests :: IO ()
runSpecTests = hspec $ describe "Test Simple State" $ do

  it "create initial state" $ do
    let state = SSI.State (\s -> (1, 1))
    SSI.runState state 0 `shouldBe` (1, 1)

  it "as functor" $ do
    let sa = SSI.State (\s -> (1, s))
        f n = n * 10 + 1
        sb = fmap f sa
    SSI.runState sb 0 `shouldBe` (11, 0)

  it "as applicative functor" $ do
    let
      sa = (pure 1) :: SSI.State Int Int
      f n = n * 100 + n * 10 + 1
      sf = SSI.State (\s -> (f, s))
      sb = sf <*> sa
      -- applicative's power is sequencing and chaining functionalities
      g n m = n * 2 + m
      sc =
        (pure g)
          <*> (pure 10 :: SSI.State Int Int)
          <*> (pure 25 :: SSI.State Int Int)
    SSI.runState sb 0 `shouldBe` (111, 0)
    SSI.runState sc 1 `shouldBe` (45, 1)

  it "wrap compute function" $ do
    let sa = (pure 1) :: SSI.State Int Int
        -- a pure function
        f x = x * 1000 + x * 100 + x * 10 + 1
        -- wrapping the pure function in the Monad context
        g x = return ((f x) * (-1))
        sb = do
          n <- sa
          -- use return() to work with pure function
          -- see state_monad_diagram.png to understand this concept
          return (f n)
        sc = do
          n <- sa
          -- not having to call return() explicitly but g is not
          -- a pure function either
          g n
    SSI.runState sb 0 `shouldBe` (1111, 0)
    SSI.runState sc 0 `shouldBe` (-1111, 0)

  it "get state (retrieve the value encapsulated)" $ do
    let sa = return 100
        sb = do
          x <- sa
          s <- SSI.get
          -- verify that I can make use of the state (-100)
          return (x + s)
    SSI.runState sb (-100) `shouldBe` (0, -100)

  it "put state (return a hardcoded value, not to compute)" $ do
    let sa = return 1
        sb = do
          x <- sa
          s <- SSI.get
          SSI.put (x * 100 + s)
          -- not computing nothing, return the input (which is 1 
          -- defined in "sa = return 1") as it is
          return x
    SSI.runState sb 1 `shouldBe` (1, 101)

runSpec :: IO ()
runSpec = do
  runSpecTests

  SimpleState.FromScratch.demo

  SimpleState.Stateless.demo

  SimpleState.Resource.demo

