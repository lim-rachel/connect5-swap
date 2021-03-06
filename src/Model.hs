{-# LANGUAGE RecordWildCards #-}
module Model where 

import Prelude hiding ((!!))
import qualified Model.Board  as Board
import qualified Model.Player as Player

-------------------------------------------------------------------------------
-- | Ticks mark passing of time: a custom event that we constantly stream
-------------------------------------------------------------------------------
data Tick = Tic | Swap

-------------------------------------------------------------------------------
-- | Top-level App State ------------------------------------------------------
-------------------------------------------------------------------------------

data State 
  = Intro 
  | Play PlayState 
  | Outro 
  
data PlayState = PS
  { psX      :: Player.Player   -- ^ player X info
  , psO      :: Player.Player   -- ^ player O info
  , psBoard  :: Board.Board     -- ^ current board
  , psTurn   :: Board.RY        -- ^ whose turn 
  , psPos    :: Board.Pos       -- ^ current cursor   
  , psResult :: Board.Result Board.Board
  , roundsTillSwap :: Int
  , swapping :: Int
  , startScreen :: Bool
  , twoPlayers :: Bool
  , inGame :: Bool
  } 

init :: PlayState
init = PS 
  { psX      = Player.human
  , psO      = Player.rando
  , psBoard  = Board.init
  , psTurn   = Board.R
  , psPos    = head Board.positions 
  , psResult = Board.Cont Board.init
  , roundsTillSwap = 5
  , swapping = 0
  , startScreen = True
  , twoPlayers = False
  , inGame = False
  }

isCurr :: PlayState -> Int -> Int -> Bool
isCurr s r c = Board.pRow p == r && Board.pCol p == c
  where 
    p = psPos s 

next :: PlayState -> Board.Result Board.Board -> PlayState
--turn not successful, state stays the same
next s Board.Retry     = s
--turn successful, update playstate board and flip turns
next s (Board.Cont b')  = (s { psBoard = b'
                             , swapping = if (roundsTillSwap s) == 0
                                      && (psTurn s) == Board.Y then 6 else (swapping s)
                             , roundsTillSwap = (case (psTurn s) of
                                    Board.R -> (roundsTillSwap s) - 1
                                    Board.Y -> roundsTillSwap s)
                             , psTurn  = Board.flipRY (psTurn s) })
-- win or draw
next s res@(Board.Win _ b') = s { psResult = res, inGame = False, psBoard = b' }
next s res@(Board.Draw  b') = s { psResult = res, inGame = False, psBoard = b' }

{--
nextBoard :: PlayState -> Board.Result a -> Either (Board.Result ()) PlayState
nextBoard s res = case res' of
                    Board.Win _ -> Left res' 
                    Board.Draw  -> Left res'
                    _           -> Right s' 
  where 
    sc'  = Score.add (psScore s) (Board.boardWinner res) 
    res' = Score.winner sc'
    s'   = s { psScore = sc'                   -- update the score
             , psBoard = mempty                -- clear the board
             , psTurn  = Score.startPlayer sc' -- toggle start player
             } --}