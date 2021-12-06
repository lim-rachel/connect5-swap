{-# LANGUAGE RecordWildCards #-}
module Model.Score where

import Model.Board (Result (..), RY (..))

-------------------------------------------------------------------------------
-- | Score --------------------------------------------------------------------
-------------------------------------------------------------------------------

data Score = Score 
  { scMax  :: Int  -- ^ total number of boards
  , scR    :: Int  -- ^ points for player R 
  , scY    :: Int  -- ^ points for player Y 
  , scD    :: Int  -- ^ drawn games 
  }
  deriving (Eq, Ord, Show)

init :: Int -> Score
init n = Score n 0 0 0

add :: Score -> Maybe RY -> Score
add sc (Just R) = sc { scR = scR sc + 1 }
add sc (Just Y) = sc { scY = scY sc + 1 }
add sc Nothing  = sc { scD = scD sc + 1 }

get :: Score -> RY -> Int
get Score {..} R = scR 
get Score {..} Y = scY 

currRound :: Score -> Int
currRound Score {..} = scR + scY + scD + 1

startPlayer :: Score -> RY
startPlayer sc 
  | even (currRound sc) = R
  | otherwise           = Y

winner :: Score -> Result () 
winner sc@Score {..}
  | scR > scY + left = Win R
  | scY > scR + left = Win Y
  | left == 0        = Draw
  | otherwise        = Cont ()
  where 
    left             = 1 + scMax - currRound sc