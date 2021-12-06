module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (borderWithLabel, hBorder, vBorder)
import Brick.Widgets.Border.Style (unicode)
import Text.Printf (printf)

import Model
import Model.Board
import Graphics.Vty hiding (dim)

-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = 
  withBorderStyle unicode $
    borderWithLabel (str (header s)) $
      vTile [ mkRow s row | row <- [1..8] ]

header :: PlayState -> String
header s = printf "Connect5-Swap Turn = %s, row = %d, col = %d" (show (psTurn s)) (pRow p) (pCol p)
  where 
    p    = psPos s

mkRow :: PlayState -> Int -> Widget n
mkRow s row = withAttr (attrName "board") $ vLimit 3 (hTile [ mkCell s row i | i <- [1..10] ]) --[mkCell s row 1, mkCell s row 2, ..., mkCell s row dim]

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell state row col 
  | isCurr state row col = withCursor raw 
  | otherwise    = raw 
  where
    raw = mkCell' state row col

withCursor :: Widget n -> Widget n
withCursor = modifyDefAttr (`withStyle` reverseVideo)

mkCell' :: PlayState -> Int -> Int -> Widget n
-- mkCell' _ r c = center (str (printf "(%d, %d)" r c))
mkCell' state row col = hLimit 6 (center (mkRY ryMb))
  where 
    ryMb      = psBoard state ! Pos row col

mkRY :: Maybe RY -> Widget n
mkRY Nothing  = blockB
mkRY (Just R) = blockR
mkRY (Just Y) = blockY

blockB, blockR, blockY :: Widget n
blockB = withAttr (attrName "empty") $
         vBox [ str "▄▉█▄"
              , str "▀▉█▀"]
blockR = withAttr (attrName "redPiece") $ 
         vBox [ str "▄▉█▄"
              , str "▀▉█▀"]
blockY = withAttr (attrName "yellowPiece") $
         vBox [ str "▄▉█▄"
              , str "▀▉█▀"]

vTile :: [Widget n] -> Widget n
vTile ne@(_:_) = vBox ne
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile ne@(_:_) = hBox ne
hTile _      = emptyWidget