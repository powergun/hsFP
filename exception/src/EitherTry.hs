module EitherTry (demo) where

import qualified Control.Exception as E
import qualified Data.Either       as DE

{-
try :: Exception e => IO a -> IO (Either e a)

https://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html#v:try
Similar to catch, but returns an Either result which is (Right a) if no exception of type e was raised, or (Left ex) if an exception of type e was raised and its value is ex. If any other type of exception is raised than it will be propogated up to the next enclosing exception handler.

for a full list of exception types, see:
http://hackage.haskell.org/package/base-4.12.0.0/docs/Control-Exception.html

experiment:
replace IOException with NoMethodError (meaning not handling
the IOException) then run the test, observe the termination with
exit code set to 1
-}
safeReadFile :: FilePath -> IO (Either E.IOException String)
safeReadFile = E.try . readFile

demo :: IO ()
demo = do
  ret <- safeReadFile "/non/existing"
  DE.either print print ret
  ret2 <- safeReadFile "/etc/passwd"
  DE.either print (print . length) ret2
