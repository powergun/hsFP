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

-- see: First Principles P/883 for a similar process / post-process
-- example, using abstract typing
demoMonad :: IO ()
demoMonad = do
  print $ runReader (process >>= postProcess) "idd"
  print $ runReader (process >>= postProcess) "idnoclip"
 where
  process :: Reader String [Int]
  process = do
    env <- ask
    if (> 4) . length $ env
      then return $ fmap ord env
      else return $ fmap ((* 100) . ord) env
  postProcess :: [Int] -> Reader String [Int]
  postProcess xs = do
    env <- ask
    return $ [length env, 0, 0] `mappend` xs

data GameSession = GameSession
  { gameName :: String
  , gameMode :: String
  } deriving (Show)

demoReaderFieldAccess = print
  $ runReader process (GameSession "nuketown" "free-for-all")
 where
  process :: Reader GameSession GameSession
  process = do
    g  <- ask
    gn <- gameName <$> ask
    gm <- gameMode <$> ask
    if gm == "free-for-all"
      then return $ GameSession gn "deathmatch"
      else return g

demo :: IO ()
demo = do
  demoFunctor
  demoApplicative
  demoMonad
  demoReaderFieldAccess

