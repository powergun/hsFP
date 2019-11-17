
module TestWrapEnv
  ( runSpec
  )
where

import qualified WrapEnv.ImplV1

runSpec :: IO ()
runSpec = WrapEnv.ImplV1.demo 
