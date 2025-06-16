{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Engine where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Border.Style (unicodeBold)
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
    { _gameBoard :: C.Board
    , _currentTurn :: C.Color
    , _selected :: Maybe C.Pos
    }

-- Generate lenses using Template Haskell
makeLenses ''St

drawUI :: St -> [Widget Name]
drawUI st = [center $ vBox rows]
  where
    rows = [hBox $ map (drawSquare st) [(x,y) | x <- [1..8]] | y <- reverse [1..8]]

drawSquare :: St -> C.Pos -> Widget Name
drawSquare st pos@(x,y) = withBorderStyle unicodeBold $
    borderWithLabel (str [colLabel x, rowLabel y]) $
    padAll 1 $
        (withAttr (attrName $ colorAttr pos) . str) $ pieceStr pos
  where
    colLabel n = ['A'..'H'] !! (n - 1)
    rowLabel n = head (show n)

    colorAttr (x, y) = if even (x + y) then "light" else "dark"
    
    pieceStr p = case C.getPiece (st ^. gameBoard) p of
                    Just ((r,_), c) -> pieceChar r c
                    Nothing         -> " "

    -- Fixed Unicode chess piece characters
    pieceChar r C.White = case r of
        C.King   -> "♔"
        C.Queen  -> "♕"
        C.Rook   -> "♖"
        C.Bishop -> "♗"
        C.Knight -> "♘"
        C.Pawn   -> "♙"
    
    pieceChar r C.Black = case r of
        C.King   -> "♚"
        C.Queen  -> "♛"
        C.Rook   -> "♜"
        C.Bishop -> "♝"
        C.Knight -> "♞"
        C.Pawn   -> "♟"

handleEvent :: BrickEvent Name () -> EventM Name St ()
handleEvent (VtyEvent (EvKey key [])) = do
    st <- get
    case key of
        KChar 'q' -> halt
        
        KChar 'r' -> put $ St
            { _gameBoard   = C.newBoard
            , _currentTurn = C.White
            , _selected    = Nothing
            }

        KEnter -> case st ^. selected of
            Just from -> do
                let moves = C.validMoves (st ^. gameBoard) from
                -- Here you would typically wait for destination input
                -- For now, we'll just clear the selection
                modify $ selected .~ Nothing
            Nothing -> return ()

        -- Add more key bindings for piece selection
        -- For example, you could use arrow keys to navigate
        KUp -> return ()    -- Navigate up
        KDown -> return ()  -- Navigate down
        KLeft -> return ()  -- Navigate left
        KRight -> return () -- Navigate right
        
        _ -> return ()

handleEvent _ = return ()

app :: App St () Name
app = App
    { appDraw         = drawUI
    , appChooseCursor = neverShowCursor
    , appHandleEvent  = handleEvent
    , appStartEvent   = return ()
    , appAttrMap      = const theMap
    }

theMap :: AttrMap
theMap = attrMap V.defAttr
    [ (attrName "light", V.white `on` V.black)
    , (attrName "dark", V.black `on` V.white)
    ]


main :: IO ()
main = do
    let initialState = St C.newBoard C.White Nothing
    _ <- defaultMain app initialState
    return ()

