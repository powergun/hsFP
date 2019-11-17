module MoveCursor.ImplV1
  ( Cursor(..)
  , Move(..)
  , moveCursor
  )
where

import qualified Control.Monad.Reader          as R
import qualified Control.Monad.Writer          as W
import qualified Data.Monoid                   as M

data Cursor = Cursor Int Int deriving (Eq, Show)

instance Semigroup Cursor where
  (Cursor x1 y1) <> (Cursor x2 y2) = Cursor (x1 + x2) (y1 + y2)

instance M.Monoid Cursor where
  mempty  = Cursor 0 0
  mappend = (<>)

data Move = North Int
          | South Int
          | West Int
          | East Int
          deriving (Show)

-- L3557
-- to convert Move to the cursor movement           
toCursor :: Move -> Cursor
toCursor (North n) = Cursor 0 (-n)
toCursor (South n) = Cursor 0 n
toCursor (West  n) = Cursor (-n) 0
toCursor (East  n) = Cursor n 0

-- use tell to update the state
-- a composition of tell . toCursor converts Move to Cursor
-- and update the state
updateCursor :: Monad m => Move -> W.WriterT Cursor m ()
updateCursor = W.tell . toCursor

-- read the current state from Reader monad and uses IO and 
-- Writer actions to move the cursor according to the supplied
-- moves
-- note how it uses lift for moving from outer monad to the 
-- inner monad
-- MY NOTES: recall
-- we should allow an operation in the embedded monad in the 
-- context of our state transformer
-- lift is exported from both Writer module and Reader module
-- using R.lift makes more sense
-- L3592
-- R.ReaderT Cursor (W.WriterT Cursor IO) ()
-- read as follows:
-- the innermost monad is IO
-- WriterT embeds IO into it; the writer state is the Cursor
-- ReaderT embeds WriterT (and IO) monads; the ReaderT has the 
--    environment Cursor
-- Essentially we start from the innermost monad and keep wrapping
-- it with another monad
-- L3592
-- to run the preceding monad, we have unwrapped the outer most
-- monad first
-- Outermost monad is ReaderT, we will unwrap it with runReaderT
-- Then next monad is WriterT, we will unwrap it with runWriterT
-- The IO monad actions are embedded innermost. When we unwrap
-- everything we are left with IO monad, which we executed in the 
-- main function, which is IO monad itself
moveCursor :: [Move] -> R.ReaderT Cursor (W.WriterT Cursor IO) ()
moveCursor ms = do -- in Reader monad

  -- L3604
  -- we can freely call actions from any Monad
  -- we need to remember to lift the actions
  -- lift() is a function which takes an action meant for one 
  -- of the inner monads, and converts it into action in outer
  -- monad

  c <- R.ask -- get the position from the environment

  -- apply some logic to the cursor before calling into the 
  -- writer monad; this is still inside reader monad
  let c' = c <> (Cursor 0 0)

  R.lift $ W.tell c' -- operates in Writer monad
                    -- update the position
  R.lift $ moveCursor' ms -- keep moving cursor
                          -- the Writer's state is updated but
                          -- there is no way to retrieve it

  -- return the Reader monad (a, s) when calling runReaderT




 where
    -- called inside Writer monad
  moveCursor' [] =
    -- inside IO monad
    W.lift $ return ()
  moveCursor' (m : ms) = do
    -- inside IO monad
    W.lift $ putStrLn $ "applying move " ++ (show m)
    updateCursor m
    moveCursor' ms
