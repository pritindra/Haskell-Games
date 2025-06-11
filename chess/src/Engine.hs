{-# LANGUAGE OverloadedStrings #-}

module Engine where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Types
import Brick.Util (on)
import Graphics.Vty.Attributes
import Graphics.Vty.Input.Events

import Lens.Micro
import Lens.Micro.TH

import qualified Data.Map as M
import qualified Graphics.Vty as V

import qualified Chess as C

data Name = BoardWidget deriving (Ord, Show, Eq)

data St = St
    { gameBoard :: C.Board
    , currentTurn :: C.Color
    , selected :: Maybe C.Pos
    }

drawUI :: St -> [Widget Name]
drawUI st = [center $ vBox rows]
  where
    rows = [hBox $ map (drawSquare st (x,y)) [1..8] | y <- reverse [1..8], let x = y]

drawSquare :: St -> C.Pos -> Int -> Widget Name
drawSquare st (x,y) _ = withBorderStyle unicodeBold $
    borderWithLabel (str [colLabel x, rowLabel y]) $
    padAll 1 $
        (withAttr (attrName $ colorAttr (x,y)) . str) $ pieceStr (x,y)
  where
    colLabel n = ['A'..'H'] !! (n - 1)
    rowLabel n = head (show n)

    colorAttr (x, y) = if even (x + y) then "light" else "dark"
    pieceStr p = case C.getPiece (gameBoard st) p of
                    Just ((r,_), c) -> pieceChar r c
                    Nothing         -> " "

    pieceChar r C.White = [V.char (9812 + rankOffset r)]
    pieceChar r C.Black = [V.char (9812 + rankOffset r - 6)]

    rankOffset C.King = 0
    rankOffset C.Queen = 1
    rankOffset C.Rook = 2
    rankOffset C.Bishop = 3
    rankOffset C.Knight = 4
    rankOffset C.Pawn = 5
handleEvent :: BrickEvent Name () -> EventM Name St ()
handleEvent (VtyEvent (EvKey key [])) = do
    st <- get
    case key of
        KChar 'q' -> halt st

        KChar 'r' -> put $ St
            { gameBoard   = C.newBoard
            , currentTurn = C.White
            , selected    = Nothing
            }

        KEnter -> case st ^. selected of
            Just from -> do
                let moves = C.validMoves (st ^. gameBoard) from
                -- Optionally wait for destination input here
                return ()
            Nothing -> return ()

        -- Piece selection with keys (like arrow keys or mapped keys)
        -- You can bind keys to select or move pieces
        _ -> return ()

handleEvent _ = return ()

app :: App St e Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return
    , appAttrMap      = const theMap
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ ("light", V.white `on` V.black)
    , ("dark", V.black `on` V.white)
    ]

main :: IO ()
main = do
    let initialState = St C.newBoard C.White Nothing
    defaultMain app initialState

