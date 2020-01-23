module FirstPrinciples.StateFromScratch (demo) where

import           Data.Bifunctor (first)
import           Data.Bool      (bool)
import           Data.Either    (either)
import           Data.Maybe     (maybe)
import           Text.Read      (readMaybe)

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap f (Moi g) = Moi (\s -> first f (g s))

instance Applicative (Moi s) where
  pure x = Moi (\s -> (x, s))
  af <*> ax =
    let g s = let (f', s') = runMoi af s
                  (x', s'') = runMoi ax s'
              in (f' x', s'')
    in Moi g

instance Monad (Moi s) where
  return = pure
  ma >>= f =
    let g s = let (x', s') = runMoi ma s
              in runMoi (f x') s' -- (f x') produces mb
    in Moi g

get :: Moi a a
get = Moi (\s -> (s, s))

put :: s -> Moi s ()
put s = Moi (\_ -> ((), s))

exec :: Moi s a -> s -> s
exec (Moi sa) s = snd . sa $ s

eval :: Moi s a -> s -> a
eval (Moi sa) s = fst . sa $ s

modify :: (s -> s) -> Moi s ()
modify f = Moi (\s -> ((), f s))

demoStateFunctor :: IO ()
demoStateFunctor = do
  let g :: String -> ([Int], String)
      g s =
        let o :: Maybe [Int]
            o = readMaybe s
        in maybe ([], s) (\ns -> (ns, "")) o
      st = Moi g
      st' = fmap ([(-1)] `mappend`) st
  print "//// State Functor"
  print $ runMoi st' "[1, 2]"
  print $ runMoi st' "iddqd"

demoStateApplicative :: IO ()
demoStateApplicative = do
  let af = pure (+ 1) -- some simple helloworld (f x) example
      ax = pure 1
      ao = af <*> ax

      -- some conditional, stream-based (f x) example
      f' :: String -> ([Int] -> [Int], String)
      f' st = maybe (take 0, st) (\_ -> (take 10, "")) ((readMaybe st) :: Maybe [Int])
      af' :: Moi String ([Int] -> [Int])
      af' = Moi f'
      x' :: [Int]
      x' = repeat 1
      ax' :: Moi String [Int]
      ax' = pure x'
      ao' = af' <*> ax'

      -- either-based state
      -- this is to prove that applicative can not model functorial
      -- dependency, but state-dependent logic can still be implemented
      -- like so:
      -- the Left/Right-ness of the state determines the `f` and `x` but
      -- I CAN NOT CHANGE THE TYPE of the state value;
      -- I can change the Left/Right-ness of the state;
      af2 = Moi (\s -> (either (\_ -> id) (\_ -> (+ 1)) s, s))
      ax2 = Moi (\s -> (either (\_ -> 0) (\x -> bool x 10 (x > 10)) s, s))
      ao2 = af2 <*> ax2
  print "//// State Applicative"
  print $ runMoi ao "iddqd"
  print $ runMoi ao' "iddqd"
  print $ runMoi ao' "[1, 2, 3]"
  print $ runMoi ao2 (Right 1 :: Either Int Int)
  print $ runMoi ao2 (Right 100 :: Either Int Int)
  print $ runMoi ao2 (Left 100 :: Either Int Int)

demoStateMonad :: IO ()
demoStateMonad = do
  print "//// State Monad"
  let g :: Int -> Either String Int
      g x = bool (Left "dead") (Right (x - 10)) (x > 0)
      f :: (Either String Int) -> Moi String (Either String Int)
      f x = return $ either (Left . id) g x
      -- ma :: Moi String (Either String Int)
      -- ma = return . Right $ 3
      -- mb :: Moi String (Either String Int)
      -- mb = ma >>= f
  print $ runMoi ((return . Right $ 3) >>=f ) ""
  print $ runMoi ((return . Right $ 3) >>= f >>= f) ""

demoGet :: IO ()
demoGet = do
  print $ runMoi get "there is a cow"

demoPut :: IO ()
demoPut = do
  print $ runMoi (put "IDDQD") "....."

demoExecEval :: IO ()
demoExecEval = do
  let st :: Moi String [Int]
      st = do
        payload <- get
        let parsed :: Maybe [Int]
            parsed = readMaybe payload
        case parsed of
          Nothing -> return [0]
          Just xs -> do
            put ""
            return xs
  print $ exec st "[1, 2]"
  print $ eval st "[1, 2]"
  print $ exec st "a, b, c"
  print $ eval st "a, b, c"

demoModify :: IO ()
demoModify = do
  print $ runMoi (modify (+1)) 0
  print $ runMoi (modify (+1) >> modify (+1)) 0

demo :: IO ()
demo = do
  demoStateFunctor
  demoStateApplicative
  demoStateMonad
  demoGet
  demoPut
  demoExecEval
  demoModify


