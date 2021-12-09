{-# LANGUAGE DeriveFunctor #-}
module Model.Board 
  ( -- * Types
    Board
  , RY (..)
  , Pos (..)
  , Result (..)

    -- * Board API
  , dim
  , (!)
  , init
  , put
  , positions
  , emptyPositions
  , boardWinner
  , flipRY

    -- * Moves
  --, up
  --, down
  , left
  , right

  , swapAllSpots
  , numRows
  , numCols
  )
  where

import Prelude hiding (init)
import qualified Data.Map as M 

-------------------------------------------------------------------------------
-- | Board --------------------------------------------------------------------
-------------------------------------------------------------------------------

type Board = M.Map Pos RY

data RY 
  = R 
  | Y
  deriving (Eq, Show)

data Pos = Pos 
  { pRow :: Int  -- 1 <= pRow <= dim 
  , pCol :: Int  -- 1 <= pCol <= dim
  }
  deriving (Eq, Ord)

(!) :: Board -> Pos -> Maybe RY 
board ! pos = M.lookup pos board

dim :: Int
dim = 3

numRows :: Int
numRows = 8

numCols :: Int 
numCols = 10

numToWin :: Int 
numToWin = 5

positions :: [Pos]
positions = [ Pos 0 c | c <- [1..numCols] ] 

emptyPositions :: Board -> [Pos]
emptyPositions board  = [ p | p <- positions, M.notMember p board]

init :: Board
init = M.empty

-------------------------------------------------------------------------------
-- | Playing a Move
-------------------------------------------------------------------------------
                 
data Result a 
  = Draw a
  | Win RY a
  | Retry 
  | Cont a
  deriving (Eq, Functor, Show)

-- 
put :: Board -> RY -> Pos -> Result Board
put board ry pos = let colNum = pCol pos in
  case dropPiece board numRows colNum of 
     Just rowNum -> result (M.insert (Pos rowNum colNum) ry board)
     Nothing -> Retry


-- find the lowest open spot in the column, starting from a certain row
dropPiece :: Board -> Int -> Int -> Maybe Int
dropPiece _     0       _     = Nothing
dropPiece board rowNum colNum =
  case M.lookup (Pos rowNum colNum) board of 
     Just _  -> dropPiece board (rowNum - 1) colNum
     Nothing -> Just rowNum

result :: Board -> Result Board
result b 
  | isFull  b    = Draw b
  | gameWon b R  = Win  R b
  | gameWon b Y  = Win  Y b
  | otherwise    = Cont b

--return true if specified player has won the game
gameWon :: Board -> RY -> Bool
gameWon b ry = or [ connected b ry line | line <- boardLines ]

--return true if the given line has `numToWin` consecutive pieces
--  for the given player
connected :: Board -> RY -> [Pos] -> Bool
connected b ry line = nConsecutive b ry line line >= numToWin

--if at least `numToWin` consecutive pieces of `color` are found
--   at any point, return that number
--if < `numToWin` found but there are some consecutive pieces found at
--   the end, return the number of consecutive pieces at the end
--if < `numToWin` found and no pieces found at the end, return 0
nConsecutive :: Board -> RY -> [Pos] -> [Pos] -> Int
nConsecutive b ry line fullLine = let (n, t) = numFromHere b ry line in 
   if n >= numToWin then n
   else case t of 
      Just []   -> if n > 0 then
                     let (nBeg, _ ) = numFromHere b ry fullLine in n + nBeg
                   else n
      Just tVal -> nConsecutive b ry tVal fullLine 
      Nothing   -> n

--(number of consecutive pieces starting from beginning, rest of the list)
numFromHere :: Board -> RY -> [Pos] -> (Int, Maybe [Pos])
numFromHere _ _  []    = (0, Just [])
numFromHere b ry (h:t) = case M.lookup h b of 
   Just c -> if c == ry then (n + 1, t') else (0, Just t)
   Nothing -> (case t of
                 [] -> (0, Nothing)
                 _  -> (0, Just t))
  where (n, t') = numFromHere b ry t

boardLines :: [[Pos]]
boardLines = rows ++ cols ++ diags 

rows, cols, diags :: [[Pos]]
rows  = [[Pos r c | c <- [1..numCols]] | r <- [1..numRows]]
cols  = [[Pos r c | r <- [1..numRows]] | c <- [1..numCols]]
diags = map convert (filter longEnough allDiags)

-- diagonal stuff --

upperRightDiags :: [[(Int, Int)]]
upperRightDiags = [(zip [1..numRows] [n..numCols]) | n <- [1..numCols]]

lowerLeftDiags :: [[(Int, Int)]]
lowerLeftDiags = [(zip [n..numRows] [1..numCols]) | n <- [2..numCols]]

upperLeftDiags :: [[(Int, Int)]]
upperLeftDiags = [(zip [1..numRows] (reverse [1..(numCols - n)])) | n <- [0..(numCols-1)]]

lowerRightDiags :: [[(Int, Int)]]
lowerRightDiags = [(zip [n..numRows] (reverse [1..numCols])) | n <- [2..numRows]]

allDiags :: [[(Int,Int)]]
allDiags = upperRightDiags ++ lowerLeftDiags ++ upperLeftDiags ++ lowerRightDiags

longEnough :: [a] -> Bool
longEnough l = length l >= numToWin

pairToPos :: (Int, Int) -> Pos
pairToPos (rowN, colN) = Pos rowN colN

convert :: [(Int, Int)] -> [Pos]
convert l = map pairToPos l

---

isFull :: Board -> Bool
isFull b = M.size b ==  numRows * numCols

-------------------------------------------------------------------------------
-- | Moves 
-------------------------------------------------------------------------------
{--
up :: Pos -> Pos 
up p = p 
  { pRow = max 1 (pRow p - 1) 
  } 

down :: Pos -> Pos
down p = p 
  { pRow = min numRows (pRow p + 1) 
  } 
--}
left :: Pos -> Pos 
left p = p 
  { pCol   = max 1 (pCol p - 1) 
  } 

right :: Pos -> Pos 
right p = p 
  { pCol = min numCols (pCol p + 1) 
  } 

boardWinner :: Result a -> Maybe RY
boardWinner (Win ry _) = Just ry
boardWinner _        = Nothing

flipRY :: RY -> RY
flipRY R = Y
flipRY Y = R

-- Swap stuff --

--swap the color of the piece at the given position
swapPiece :: Pos -> Board -> Board
swapPiece pos board =
  case M.lookup pos board of 
     Just ry  -> M.insert pos (flipRY ry) board
     Nothing  -> board

--swap the colors of the the pieces in the given row number
swapRow :: Int -> Board -> Board
swapRow rowNum board = foldr swapPiece board [(Pos rowNum colNum) | colNum <- [1..numCols]]

--swap the colors of all the pieces on the board
swapAllSpots :: Board -> Board
swapAllSpots board = foldr swapRow board [1..numRows]



---