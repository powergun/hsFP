
module TestMaybeTrans (demo) where

import qualified MaybeTrans.PasswordValidation as Sut

demo :: IO ()
demo = do
    demoGetPassphraseBaseImpl

demoGetPassphraseBaseImpl :: IO ()
demoGetPassphraseBaseImpl = do
    let badPass = return "123"
    let goodPass = return "Iddqd123!"
    failed <- Sut.getPassphraseBaseImpl badPass
    success <- Sut.getPassphraseBaseImpl goodPass
    print failed
    print success
