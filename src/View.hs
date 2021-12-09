module View (view) where

import Brick
import Brick.Widgets.Center (center)
import Brick.Widgets.Border (border)
import Brick.Widgets.Border.Style (unicodeRounded)

import Model
import Model.Board


-------------------------------------------------------------------------------
view :: PlayState -> [Widget String]
-------------------------------------------------------------------------------
view s = [view' s]

view' :: PlayState -> Widget String
view' s = if startScreen s then titleScreen s else
  hTile [
    padRight (Pad 4) $ vTile [
      vLimit 2 $ padLeftRight 1 $ mkOutsideRow s,
      withAttr (attrName "board") $
      --withBorderStyle unicode $ border $
        padTop (Pad 1) $ padLeftRight 1 $
          vTile [ mkRow s row | row <- [1..numRows] ]
    ],
    padTop (Pad 1) $ vTile [mkTitle s, mkSwapCount s, mkGameStatus s, mkKey]
  ]

titleScreen :: PlayState -> Widget n
titleScreen s = center $ (center $ mkTitle s
        <=> (padTop (Pad 1) $ withBorderStyle unicodeRounded $ border $
             vLimit 3 $ padLeftRight 1 $ hLimit 12 $
               vBox [ center $ str "Select",
                      center $ str "[1] 1-player",
                      center $ str "[2] 2-player" ]))


mkRow :: PlayState -> Int -> Widget n
mkRow s row = vLimit 3 (hTile [ mkCell s row i | i <- [1..numCols] ])

mkOutsideRow :: PlayState -> Widget n 
mkOutsideRow s = vLimit 2 $ (hTile [ mkOutsideCell s i | i <- [1..numCols] ])

mkOutsideCell :: PlayState -> Int -> Widget n
mkOutsideCell state col 
  | isCurr state 0 col = withCursor state (mkCell state 0 col)
  | otherwise        = hLimit 6 $ center $ vBox [ str "    "
                                                , str "    "]
  
--cursor over column, indicate with outside piece
withCursor :: PlayState -> Widget n -> Widget n
withCursor state = forceAttr (attrName color)
  where
    color = (case (psTurn state) of
     R -> "red"
     Y -> "yellow")

mkCell :: PlayState -> Int -> Int -> Widget n
mkCell state row col = hLimit 6 (center (mkRY ryMb))
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
blockR = withAttr (attrName "red") $ 
         vBox [ str "▄▉█▄"
              , str "▀▉█▀"]
blockY = withAttr (attrName "yellow") $
         vBox [ str "▄▉█▄"
              , str "▀▉█▀"]

vTile :: [Widget n] -> Widget n
vTile ne@(_:_) = vBox ne
vTile _      = emptyWidget

hTile :: [Widget n] -> Widget n
hTile ne@(_:_) = hBox ne
hTile _      = emptyWidget

mkTitle :: PlayState -> Widget n 
mkTitle s = withBorderStyle unicodeRounded $ border $ padLeftRight 1 $
  hLimit 14 $ vLimit 4 $ vBox [ str "  connect5  ", mkSwapColors s]

--swap colors in "swap" word in title
mkSwapColors :: PlayState -> Widget n 
mkSwapColors s = if m == 0 then hBox [mkS, mkW, mkA, mkP]
  else hBox [a1 mkS, a2 mkW, a1 mkA, a2 mkP]
 where
  m  = swapping s
  a1 = withAttr (attrName (if (m `mod` 2) == 0 then "red" else "yellow"))
  a2 = withAttr (attrName (if (m `mod` 2) == 0 then "yellow" else "red"))


mkS, mkW, mkA, mkP :: Widget n
mkS = vBox [ str "┌─┐",
             str "└─┐",
             str "└─┘" ]
mkW = vBox [ str "┬ ┬",
             str "│││",
             str "└┴┘" ]
mkA = vBox [ str "┌─┐",
             str "├─┤",
             str "┴ ┴" ]
mkP = vBox [ str "┌─┐",
             str "├─┘",
             str "┴  " ]

mkSwapCount :: PlayState -> Widget n 
mkSwapCount s = padTop (Pad 1) $
  withBorderStyle unicodeRounded $ border $ padLeftRight 1 $
  str ("Rounds until swap: " ++ show (roundsTillSwap (s)))

mkGameStatus :: PlayState -> Widget n
mkGameStatus s =
  withBorderStyle unicodeRounded $ border $ padLeftRight 1 $ txt 
  where
    txt = if inGame s then
            (if swapping s > 0 then str ("Swapping...")
             else str ("Player " ++ (show (psTurn s)) ++ "'s turn"))
          else case (psResult s) of
            Draw _   -> str ("Draw")
            Win ry _ -> ( case ry of
                  R -> withAttr (attrName "red") $ str ("Red wins!")
                  Y -> withAttr (attrName "yellow") $str ("Yellow wins!"))
            _ -> str (" ")

mkKey :: Widget n 
mkKey = 
  padTop (Pad 2) $
  withBorderStyle unicodeRounded $ border $ padLeftRight 1 $ 
  vBox [ str "⇦ ⇨    change columns",
         str "Enter  drop piece",
         str "r      restart",
         str "Esc    exit game"
  ]