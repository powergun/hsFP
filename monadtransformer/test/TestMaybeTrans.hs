{-# LANGUAGE TemplateHaskell #-}

module TestMaybeTrans (demo) where

import MaybeTrans.ImplV1
import MaybeTrans.PasswordValidation

demo :: IO ()
demo = do
    demoGetPassphraseBaseImpl
    demoGetPassphraseMaybeT
    demoAskPassphraseMaybeT

demoGetPassphraseBaseImpl :: IO ()
demoGetPassphraseBaseImpl = do
    let badPass = return "123"
    let goodPass = return "Iddqd123!"
    failed <- getPassphraseBaseImpl badPass
    success <- getPassphraseBaseImpl goodPass
    print failed
    print success

demoGetPassphraseMaybeT :: IO ()
demoGetPassphraseMaybeT = do
  let badPass = return "123"
  let goodPass = return "Iddqd123!"
  failed <- runMaybeT $ getPassphraseMaybeT badPass
  success <- runMaybeT $ getPassphraseMaybeT goodPass
  print failed
  print success 


demoAskPassphraseMaybeT :: IO ()
demoAskPassphraseMaybeT = do
  let badPass = return "123"
  let goodPass = return "Iddqd123!"
  runMaybeT $ askPassphraseMaybeT badPass
  runMaybeT $ askPassphraseMaybeT goodPass
  return ()