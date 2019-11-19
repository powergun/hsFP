module TestDirWalker (demo) where

import qualified DirWalker.BaseImpl
import qualified DirWalker.WriterImpl

demo :: IO ()
demo = do
    -- explicitly build up a result
    ents1 <- DirWalker.BaseImpl.countEntriesTrad "/var/tmp/sut"
    print ents1

    -- use writer monad (WriterT on top of IO)
    ents2 <- DirWalker.WriterImpl.countEntries "/var/tmp/sut"
    print ents2
