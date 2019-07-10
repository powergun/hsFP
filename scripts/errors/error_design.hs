#!/usr/bin/env stack runghc

-- real world haskell P/250
-- we've established that error is for disasters, but we're still 
-- using it in our apis. In that case, malformed input should be
-- rejected but not turned into a big deal. What would be a better
-- way to handle this?
-- we can encode the possibility of failure in the type signature 
-- of api using the predefined Either type

type InputError = String

-- a value returned by thereIs will now be either Left "an error 
-- message" or Right "..."
-- this return type forces our callers to deal with the possibility 
-- of error (or run into non-exhaustive pattern)
thereIs :: String -> Either InputError String
thereIs x
  | null x = Left "empty string!"
  | otherwise = Right x

demoThereIs :: String -> IO ()
demoThereIs x = do
  let va = thereIs x
  case va of
    -- experiment: comment out the following line and run
    -- will get an error:
    -- Non-exhaustive patterns in case
    -- THOUGHT: the Left case should be covered in unit test!
    Left err -> print $ "ERROR: " ++ err
    Right s  -> print $ "DONE: " ++ s

main :: IO ()
main = do
  demoThereIs ""
  demoThereIs "a cow"
