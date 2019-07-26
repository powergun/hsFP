#!/usr/bin/env stack runghc

import Data.Char (ord)
import Data.List (uncons)
import Control.Applicative ((<$>))

-- motivation:
-- this is a simplified version of the PGM file parser example
-- from book, real world haskell P/280-290
-- this is to demo the basic mechanics of ParseState and functor

-- what is different to the book example is that:

-- input is changed to plain String, e.g. given a string "thereisacow"
-- output is changed to ascci char, e.g. return the ascii code as
-- Int for each char;
-- add one more bail condition, that is empty char is considered 
-- bad character and will terminate the parsing function

type ErrMsg = String

data ParseState = ParseState {
  string :: String
, offset :: Int
} deriving (Show)

-- recall Brian Beckman's video:
-- monad is to compose small functions to create complex function
-- compose parsers that deal with different characters to a complex
-- parser
newtype Parse a = Parse {
  runParse :: ParseState -> Either ErrMsg (a, ParseState)
}

parse :: Parse a -> String -> Either ErrMsg a
parse parser initState =
  case (runParse parser (ParseState initState 0)) of
    Left errMsg       -> Left errMsg
    Right (result, _) -> Right result

-- a in this case stands for "anything"
identity :: a -> Parse a
identity anything = Parse $ 
  \state -> Right (anything, state)

getState :: Parse ParseState
getState = Parse $
  \state -> Right (state, state)

-- WTF is ()
-- https://stackoverflow.com/questions/33112439/what-does-mean-in-haskell
-- 0-tuple; the void type (recall udemy course)
putState :: ParseState -> Parse ()
putState state = Parse $
  \_ -> Right ((), state)

(==>) :: Parse a -> (a -> Parse b) -> Parse b
firstParser ==> secondParser =
  Parse chainedParser
    where 
      chainedParser initState =
        case runParse firstParser initState of
          Left err -> 
            Left err
          Right (firstResult, newState) ->
            runParse (secondParser firstResult) newState

bail :: String -> Parse a
bail err = 
  Parse (\s -> Left ("char offset " ++ show (offset s) ++ ": " ++ err))

parseChar :: Parse Char
parseChar =
  getState ==> \initState ->
    case uncons (string initState) of 
      --  get next char
      Nothing -> 
        bail "no more input"
      Just (' ', _) ->
        bail "invalid character ' '"
      Just (char, remainder) ->
        -- has next char
        putState newState ==> \_ -> identity char
                            -- the next call to getState will
                            -- retrieve the new state
        where
          newState = initState { string = remainder, 
                                 offset = newOffset }
          newOffset = offset initState + 1

-- recall fmap "extends" the type-functionality of the given
-- parser using f as the channel, and produces a new parser that 
-- works with a different type
-- it is implemented using chaining to have f works on the result 
-- of a parsing, while leaving the state changed
instance Functor Parse where
  fmap f parser =
    parser ==> \result -> identity (f result)

c2i :: Char -> Int
c2i = ord

parseInt :: Parse Int
parseInt = c2i <$> parseChar

peekChar :: Parse (Maybe Char)
-- fst: recall fst extracts the first element of a pair
-- https://hoogle.haskell.org/?hoogle=fst
-- uncons: Decompose a list into its head and tail. If the list 
--         is empty, returns Nothing.
-- https://hackage.haskell.org/package/base-4.12.0.0/docs/Data-List.html#v:uncons
peekChar = fmap (fmap fst . uncons . string) getState
--                        ^^^^^^^^^^^^^^^^^^ returns a "container"
-- "fmap fst uncons" has different meaning: it means to work on 
-- uncons as if it is a container instead of work on the RESULT
-- OF uncons
-- real world haskell P/291
-- notice that peekChar makes two calls to fmap. This is necessary
-- because the type Parse (Maybe a) is a functor within a functor 
-- We thus have to lift a function twice to "get it into" the 
-- inner function.
-- MY NOTE: it makes sense now, there are two "containers", the
-- outer one being the ParseState, the inner being a pair returned 
-- by uncons
-- chew on this:
-- real world haskell P/294
-- functor offers a generalized way to map over a parameterized type

parseWhile :: (Char -> Bool) -> Parse [Char]
parseWhile f =
  -- peekChar's type is Just Char; the functor makes it Just Bool
  -- because f is (Char -> Bool)
  (fmap f <$> peekChar) ==> \mp ->
    if mp == Just True
    then parseChar ==> \c ->
      -- apply (c:) the result but leaving state unchanged
      (c:) <$> parseWhile f
    else
      identity []

-- not using functor
parseWhile_ :: (Char -> Bool) -> Parse [Char]
parseWhile_ f =
  peekChar ==> nextStep
  where
    nextStep mc = case mc of
      Nothing -> identity [] -- condition to terminate
      Just c | f c       -> parseChar ==> parseMore
             | otherwise -> identity []
    -- cs is [], then [c], then [c ..]
    parseMore c = parseWhile_ f ==> \cs -> identity (c:cs)

demoCreateParseState :: IO ()
demoCreateParseState = do
  print "//// demo create ParseState"
  print $ ParseState "thereisacow" 0

demoGetStatePutState :: IO ()
demoGetStatePutState = do
  print "//// demo identity, getState, putState"
  print $ parse (identity 123) "thereisacow"
  print $ parse getState "thereisacow"
  print $ parse (putState (ParseState "iddqd" 12)) "thereisacow"

demoChainParsers :: IO ()
demoChainParsers = do
  print "//// demo chain parsers to create new parser"
  -- even though the initial state is (thereisacow, 0)
  -- putState successfully changes it to (iddqd, 12)
  -- it is useless but demonstrates the gist of state manipulation
  print $ parse ((putState (ParseState "iddqd" 12)) ==> (\_ -> getState)) "thereisacow"
  print $ parse (parseChar ==> \_ -> parseChar) "iddqd"
  
demoParseChar :: IO ()
demoParseChar = do
  print "//// demo parseChar"
  -- retrieve the first char
  print $ parse parseChar "thereisacow"
  -- this skips the first char and retrieve the second char
  print $ parse (parseChar ==> \_ -> parseChar) "thereisacow"
  -- catch invalid character (space char) and report parsing 
  -- position
  print $ parse (parseChar ==> \_ -> parseChar) "t hereisacow"

demoFunctorInstance :: IO ()
demoFunctorInstance = do
  print "//// demo Parse as a functor instance"
  print $ parse (id <$> parseChar) "iddqd"
  print $ parse (ord <$> parseChar) "iddqd"
  print $ parse ( fmap ((*100) . ord)     parseChar) "iddqd"
  print $ parse ((fmap (*100) . fmap ord) parseChar) "iddqd"

demoParseInt :: IO ()
demoParseInt = do
  print "//// demo parseInt (functor)"
  print $ parse parseInt "KKND"
  print $ parse parseInt " foo"

demoPeekChar :: IO ()
demoPeekChar = do
  print "//// demo peek char"
  print $ parse peekChar ""
  print $ parse peekChar "a"
  -- peek does not change the state 
  print $ parse (peekChar ==> (\_ -> parseChar)) "a"

demoParseWhile :: IO ()
demoParseWhile = do
  print "//// demo parseWhile()"
  print $ parse (parseWhile (\c -> True)) "thereisacow"
  -- give it a string with invalid character (space char), expect
  -- the error to propagate
  print $ parse (parseWhile (\_ -> True)) "there isacow"
  -- set | to be the terminate character
  print $ parse (parseWhile (\c -> c `notElem` ['|'])) "there|is|acow"

demoParseWhileNonFunctor :: IO ()
demoParseWhileNonFunctor = do
  print "//// demo non-functor parseWhile_"
  -- use the non-functor version 
  print $ parse (parseWhile_ (\_ -> True)) "thereisacow"
  print $ parse (parseWhile_ (\_ -> True)) "there isacow"
  print $ parse (parseWhile_ (\c -> c `notElem` ['|'])) "there|is|acow"
  
main :: IO () 
main = do
  demoCreateParseState
  demoGetStatePutState
  demoChainParsers
  demoParseChar
  demoFunctorInstance
  demoParseInt
  demoPeekChar
  demoParseWhile
  demoParseWhileNonFunctor


