module RedoFold
  ( demo
  )
where

import           Data.Functor.Constant          ( Constant(..)
                                                , getConstant
                                                )
import           Data.Monoid                    ( Sum )

foldMap' f xs = getConstant $ traverse (Constant . f) xs

demo :: IO ()
demo = print $ foldMap' (* (-1)) ([1, 2, 3, 4, 5] :: [Sum Int])
