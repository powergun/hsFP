module Exercise2 (main) where

returnMaybe = pure

main
  | returnMaybe "Hello" == Just "Hello" = putStrLn "Correct!"
  | otherwise = putStrLn "Incorrect, please try again"
