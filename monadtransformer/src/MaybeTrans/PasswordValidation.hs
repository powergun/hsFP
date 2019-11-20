module MaybeTrans.PasswordValidation
    ( getPassphraseBaseImpl
    )
    where

import           Data.Char

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
