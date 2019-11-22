{-# LANGUAGE TemplateHaskell #-}

module MaybeTrans.PasswordValidation
    ( getPassphraseBaseImpl
    , getPassphraseMaybeT
    , askPassphraseMaybeT
    )
    where

import           Data.Char
import qualified Control.Monad as M
import qualified Control.Monad.Trans as Mt
import MaybeTrans.ImplV1

-- to test:
-- stack test
-- (in ghci) getPassphraseBaseImpl getLine
getPassphraseBaseImpl :: IO (String) -> IO (Maybe String)
getPassphraseBaseImpl getter = do
    -- NOT using Maybe as a monad here (but only a return type)
    s <- getter
    if isValid s then return $ Just s
                 else return Nothing

-- The validation test could be anything we want it to be.
isValid :: String -> Bool
isValid s = length s >= 8
            && any isAlpha s
            && any isNumber s
            && any isPunctuation s

-- because MaybeT IO is an instance of Alternative, checking
-- for passphrase validity can be taken care of by a guard 
-- statement, which will return empty (i.e. IO Nothing) in case
-- of a bad passphrase
getPassphraseMaybeT :: IO String -> MaybeT IO String
getPassphraseMaybeT getter = do
  s <- Mt.lift getter
  -- Alternative provides guard
  -- NOTE: use hoogle to search for which package exports guard()
  -- it turns out to be Control.Monad
  M.guard (isValid s)
  return s

-- caller function takes advantage of MaybeT:
-- we do not have to manually check whether the result is 
-- Nothing or Just..., the bind operator takes care of that 
askPassphraseMaybeT :: IO String -> MaybeT IO ()
askPassphraseMaybeT getter = do
  Mt.lift $ putStrLn "Insert your new passphrase: "
  value <- getPassphraseMaybeT getter
  Mt.lift $ putStrLn "Done. Storing in database."

-- test this in ghci!!
-- runMaybeT askPassphraseGhci 
-- Insert your new passphrase: 
-- 1
-- 124
-- 132321h!
-- Done. Storing in database.
-- Just ()
-- see also:
-- What I Wish I Knew When Learning Haskell 
-- http://dev.stephendiehl.com/hask/
askPassphraseGhci :: MaybeT IO ()
askPassphraseGhci = do
  Mt.lift $ putStrLn "Insert your new passphrase: "
  -- keep asking until getting legit answer
  value <- M.msum . repeat . getPassphraseMaybeT $ getLine
  Mt.lift $ putStrLn "Done. Storing in database."

-- Test this in ghci
patternMatch :: IO ()
patternMatch = do
  let readPass1 :: MaybeT IO String
      readPass1 = do
        s <- Mt.lift getLine
        M.guard (isValid s)
        return s
      readPass2 :: MaybeT IO String
      readPass2 = do
        s <- Mt.lift getLine
        M.guard (isValid s)
        return s
  pw <- runMaybeT $ do
    p1 <- readPass1
    p2 <- readPass2
    return (p1, p2)
  case pw of 
    Nothing -> print "bail out!"
    Just (p1, p2) -> print (p1 ++ " | " ++ p2) 
