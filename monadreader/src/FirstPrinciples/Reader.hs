module FirstPrinciples.Reader
  ( demo
  )
where

import           Data.Char                      ( ord )

newtype Reader r a = Reader {
  runReader :: r -> a
}

instance Functor (Reader r) where
  fmap f (Reader ra) = Reader (f . ra) -- Reader $ \r -> f (ra r)

ask :: Reader a a
ask = Reader id

demoFunctor :: IO ()
demoFunctor = do
  let r  = Reader (+ 100)
      r' = fmap (const 1) r
  print $ runReader r 123
  print $ runReader r' 123

instance Applicative (Reader r) where
  pure = Reader . const
  (Reader rf) <*> (Reader ra) = Reader $ rf <$> id <*> ra

demoApplicative :: IO ()
demoApplicative = do
  let rf = Reader (+)
      ra = pure 100
      r  = rf <*> ra
  print $ runReader r 10

instance Monad (Reader r) where
  return = pure
  (Reader ra) >>= f =
    let rf r = case f . ra $ r of
          Reader rb -> rb r
    in  Reader rf

demoMonad :: IO ()
demoMonad = do
  print $ runReader process "idd"
  print $ runReader process "idnoclip"
 where
  process :: Reader String [Int]
  process = do
    env <- ask
    if (> 4) . length $ env
      then return $ fmap ord env
      else return $ fmap ((* 100) . ord) env

demo :: IO ()
demo = do
  demoFunctor
  demoApplicative
  demoMonad

