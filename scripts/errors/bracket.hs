#!/usr/bin/env stack runghc

{-# LANGUAGE ScopedTypeVariables #-}

-- real world haskell P/261
-- the acquire-use-release cycle
-- we need hClose to always be called if openFile succeeds,
-- the bracket function takes three actions as argument.

-- see find / BetterPredicate.hs for example

import System.IO
import Control.Exception

getFileSize :: FilePath -> IO ()
getFileSize path = do
  handle 
    (\(e :: SomeException) -> print "failed...") 
    (bracket acquire release use)
  -- bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
  -- 1st: acquires a resource
  -- 2nd: releases the resource
  -- 3nd: runs in bettern (the "use" action)
  
  where
    -- if the acquire action succeeds, the release action is always 
    -- called. This guarantees that the resource will always be 
    -- released.
    acquire = do
      h <- openFile path ReadMode
      print "opened!"
      return h

    -- if an exception occurs while the use action is executing
    -- bracket calls the release action and re-throws the exception
    -- if the use action succeeds, bracket calls the release action
    -- and returns the value returned by the use action
    release h = do
      print "closing!"
      hClose h

    -- the use and release actions are each passed the resource
    -- acquired by the acquire action
    use h = do
      print "using!"
      sz <- hFileSize h
      print $ "success: " ++ (show sz)
  
main :: IO ()
main = do
  -- "failed..."
  -- "failed..."
  -- "opened!"
  -- "using!"
  -- "success: 782"
  -- "closing!"
  -- "opened!"
  -- "using!"
  -- "closing!"
  -- "failed..."

  -- fail at acquire action
  getFileSize "."
  -- call fail handler: "failed..."
  getFileSize "/non/existing/file"
  -- call fail handler: "failed..."

  -- fully success
  getFileSize "error.hs"
  -- open use close

  -- fail at use
  getFileSize "/dev/urandom"
  -- open use(exception) close, call fail handler: "failed..."
