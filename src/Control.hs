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
  AppEvent Tic                         -> nextS s =<< liftIO (play Y s True)
  AppEvent Swap                        -> Brick.continue (maybeSwap s)
  T.VtyEvent (V.EvKey V.KEnter _)      -> nextS s =<< liftIO (play (psTurn s) s False)
  T.VtyEvent (V.EvKey (V.KChar '1') _) -> Brick.continue (setGameMode False s)
  T.VtyEvent (V.EvKey (V.KChar '2') _) -> Brick.continue (setGameMode True s)
  T.VtyEvent (V.EvKey (V.KChar 'r') _) -> Brick.continue Model.init
  T.VtyEvent (V.EvKey V.KLeft _)       -> Brick.continue (move left  s)
  T.VtyEvent (V.EvKey V.KRight _)      -> Brick.continue (move right s)
  T.VtyEvent (V.EvKey V.KEsc _)        -> Brick.halt s
  _                                    -> Brick.continue s

-------------------------------------------------------------------------------
move :: (Pos -> Pos) -> PlayState -> PlayState
-------------------------------------------------------------------------------
move f s = if inGame s then s { psPos = f (psPos s) } else s

-------------------------------------------------------------------------------
play :: RY -> PlayState -> Bool -> IO (Result Board)
-------------------------------------------------------------------------------
play ry s fromTic = 
  if fromTic && twoPlayers s then return Retry
  else (if (swapping s) > 0 then return Retry
  else (if inGame s then
       (if psTurn s == ry then do
                r <- put (psBoard s) ry <$> getPos ry s
                return r
        else return Retry)
  else return Retry
  ))

getPos :: RY -> PlayState -> IO Pos
getPos ry s = getStrategy ry s (psPos s) (psBoard s) ry

getStrategy :: RY -> PlayState -> Strategy 
getStrategy R s = plStrat (psX s)
getStrategy Y s = plStrat (psO s)

-------------------------------------------------------------------------------
nextS :: PlayState -> Result Board -> EventM n (Next PlayState)
-------------------------------------------------------------------------------
nextS s b = continue (next s b)

-- update the playstate with swapped colors
controlSwap :: PlayState -> PlayState 
controlSwap s = s { psBoard = swapAllSpots (psBoard s),
                    swapping = (swapping s) - 1,
                    roundsTillSwap = 5 }

-- make the color-swapping letters happen if needed
maybeSwap :: PlayState -> PlayState 
maybeSwap s =
  if m == 0 then s
  else (if m == 1 then controlSwap s
  else (s { swapping = m - 1 }))
  where m = swapping s

-- False means 1 player, True means 2 players
setGameMode :: Bool -> PlayState -> PlayState 
setGameMode mode s = if startScreen s then
                       s { twoPlayers = mode, startScreen = False, inGame = True,
                           psO = if mode then human else rando }
                     else s