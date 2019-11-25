module AlternativeDemo.UseCases (demo) where

-- searched on hoogle to find out which package exports asum()
-- it turned out to be Data.Foldable
import qualified Control.Applicative as Ca
import qualified Control.Monad       as M
import qualified Data.Foldable       as Df

asumDemo :: IO ()
asumDemo = do
    let r = Df.asum [[1], [3, 4, 5], [], [0]]
    print r

-- source
-- https://en.wikibooks.org/wiki/Haskell/Alternative_and_MonadPlus
-- guard in this case offers the same semantic as list-comprehension
-- (note, this is *different* to zip!)
listGuard :: (Num a, Ord a) => [a] -> [a] -> [(a, a)]
listGuard lhs rhs = do
    l <- lhs
    r <- rhs
    M.guard ((l + r) > 10)
    return (l, r)

demo :: IO ()
demo = do
    let r1 = listGuard [1, 10, 0] [2, 20, -14]
    print r1
    asumDemo
    return ()
