module Control where

import Brick hiding (Result)
import qualified Graphics.Vty as V
import qualified Brick.Types as T

import Model
import Model.Board
import Control.Monad.IO.Class (MonadIO(liftIO))
import Model.Player
import Control.Concurrent (threadDelay)

-------------------------------------------------------------------------------

control :: PlayState -> BrickEvent n Tick -> EventM n (Next PlayState)
control s ev = case ev of 
  AppEvent Tic                    -> nextS s =<< liftIO (play Y s)
  AppEvent Swap                   -> Brick.continue (maybeSwap s)
  T.VtyEvent (V.EvKey V.KEnter _) -> nextS s =<< liftIO (play R s)
  --T.VtyEvent (V.EvKey (V.KChar 's') _) -> Brick.continue (controlSwap s)
  T.VtyEvent (V.EvKey V.KLeft _)  -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _) -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)   -> Brick.halt s
  _                               -> Brick.continue s -- Brick.halt s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = s { psPos = f (psPos s) }

-------------------------------------------------------------------------------
play :: RY -> PlayState -> IO (Either Board (Result Board))
-------------------------------------------------------------------------------
play ry s = 
  if (swapping s) > 0 then return (Right Retry)
  else (if psTurn s == ry then do
                r <- put (psBoard s) ry <$> getPos ry s
                return (Right r)
        else return (Right Retry))

getPos :: RY -> PlayState -> IO Pos
getPos ry s = getStrategy ry s (psPos s) (psBoard s) ry

getStrategy :: RY -> PlayState -> Strategy 
getStrategy R s = plStrat (psX s)
getStrategy Y s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Either Board (Result Board) -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = case b of
  Left  b' -> continue (s { roundsTillSwap = 5, psBoard = b' }) --swapped, reset counter
  Right b' -> (case next s b' of
     Right s' -> continue s' --continue playing the game
     Left res -> halt (s { psResult = res }) ) 

-- update the playstate with swapped colors
controlSwap :: PlayState -> PlayState 
controlSwap s = s { psBoard = swapAllSpots (psBoard s),
                    swapping = (swapping s) - 1,
                    roundsTillSwap = 5 }

maybeSwap :: PlayState -> PlayState 
maybeSwap s =
  if m == 0 then s
  else (if m == 1 then controlSwap s
  else (s { swapping = m - 1 }))
  where m = swapping s