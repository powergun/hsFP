
module Local.Simple (triplet) where

import qualified Control.Monad.Reader as Mr

-- source:
-- http://book.realworldhaskell.org/read/monad-transformers.html

-- local function:
-- It temporarily modifies the current environment using the
-- r -> r function, and executes its action in the modified environment.

-- also, this example shows how to use Reader monad (not its transformer)

-- according to: http://hackage.haskell.org/package/mtl-2.2.2/docs/Control-Monad-Reader.html
-- type Reader r = ReaderT r Identity
-- therefore `Mr.Reader String String` can also be written as:
-- `Mr.ReaderT String Identity [Char]`
-- the second form is what ghci :t prints out
-- NOTE: the hackage page uses the form that supports extension
-- (not hardcoding "a" in the type parameter list)
action :: Mr.Reader String String
action = do
    txt <- Mr.ask
    return $ "set " ++ txt

tripletImpl :: Mr.Reader String (String, String)
tripletImpl = do
    hd <- action
    tl <- Mr.local (const "idkfa") action
    return (hd, tl)

triplet :: String -> IO (String, String)
triplet txt = do
    return $ Mr.runReader tripletImpl txt
