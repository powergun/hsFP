module TestDirWalker (demo) where

import qualified DirWalker.BaseImpl
import qualified DirWalker.WriterImpl
import qualified DirWalker.TransStackImpl

demo :: IO ()
demo = do
    -- explicitly build up a result
    ents1 <- DirWalker.BaseImpl.countEntriesTrad "/var/tmp/sut"

    -- use writer monad (WriterT on top of IO)
    ents2 <- DirWalker.WriterImpl.countEntries "/var/tmp/sut"
    print . ("BaseImpl == WriterImpl: " ++) . show $ ents1 == ents2

    -- use a transformer stack 
    ents3 <- DirWalker.TransStackImpl.countEntries 99 "/var/tmp/sut"
    print . ("BaseImpl == TransStackImpl: " ++) . show $ ents1 == ents3
