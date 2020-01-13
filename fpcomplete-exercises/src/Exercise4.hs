module Exercise4 (main) where

import           Control.Applicative ((<$>), (<*>))
import           Text.Read           (readMaybe)

displayAge maybeAge =
    case maybeAge of
        Nothing  -> putStrLn "You provided invalid input"
        Just age -> putStrLn $ "In that year, you will be: " ++ show age

yearDiff futureYear birthYear = (-) <$> const birthYear <*> const futureYear $ undefined

main
    | yearDiff 5 6 == 1 = putStrLn "Correct!"
    | otherwise = putStrLn "Please try again"
