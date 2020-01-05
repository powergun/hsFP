module FirstPrinciples.FunctorInTermsOfMonad (demo) where

-- source: first principles P/756
-- to implement functor's fmap in terms of monad
proof :: (Monad m, Num a, Eq (m a)) => m a -> Bool
proof m =
  let asFunctor = (+ 1) <$> m
      asMonad = m >>= return . (+ 1)
  in asFunctor == asMonad

demo :: IO ()
demo = do
  print $ proof [1, 2, 3]
