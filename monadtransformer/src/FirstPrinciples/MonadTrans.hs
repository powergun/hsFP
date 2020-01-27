{-# LANGUAGE OverloadedStrings #-}

module FirstPrinciples.MonadTrans (demo) where

import           Control.Monad.Trans.Class
import           Data.Monoid               (mconcat)
import           Web.Scotty

makeItCompile = scotty 3000 $ do
  get "/:word" $ do -- ActionM
    beam <- param "word"
    (lift :: IO a -> ActionM a) (putStrLn "hello") -- run IO inside ActionM
                            -- ActionM is, ActionT Text IO
                            -- ActionT e m a
                            -- IO a -> ActionT Text IO a (ActionM a)

    (lift :: (Monad m, MonadTrans t) => m a -> t m a) (putStrLn "")
    (lift :: (MonadTrans t) => IO a -> t IO a) (putStrLn "")
    (lift :: IO a -> ActionM a) (putStrLn "")
    (lift :: IO () -> ActionM ()) (putStrLn "")


    html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

demo :: IO ()
demo = return ()
