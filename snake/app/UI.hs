{-# LANGUAGE OverloadedStrings #-}
module UI where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Class (modify, put, get)
import Control.Concurrent (threadDelay, forkIO)
import Data.Maybe()

import Snake
import Start (start)

import Brick
import Brick.AttrMap (attrName)
import Brick.BChan (newBChan, writeBChan)
import Brick.Widgets.Border (borderWithLabel)
import Brick.Widgets.Border.Style (unicodeBold)
import Brick.Widgets.Center
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens ((^.),(&))
import qualified Graphics.Vty as V
import Graphics.Vty.CrossPlatform (mkVty)
import qualified Data.Sequence as S
import Linear.V2 (V2(..))

-- Types

data Tick = Tick

type Name = ()

data Cell = Snake1 | Snake2 | Food | Empty | SnakeHead1 | SnakeHead2

-- App definition

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return ()
          , appAttrMap = const theMap
          }

main :: IO ()
main = do
  p2 <- start
  chan <- newBChan 10
  _ <- forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay 100000 -- Game speed
  g <- initGame p2
  let builder = mkVty V.defaultConfig
  initialVty <- builder
  void $ customMain initialVty builder (Just chan) app g

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name Game ()
handleEvent (AppEvent Tick)                       = modify step
handleEvent (VtyEvent (V.EvKey V.KUp []))         = modify $ turn1 North
handleEvent (VtyEvent (V.EvKey V.KDown []))       = modify $ turn1 South
handleEvent (VtyEvent (V.EvKey V.KRight []))      = modify $ turn1 East 
handleEvent (VtyEvent (V.EvKey V.KLeft []))       = modify $ turn1 West 
handleEvent (VtyEvent (V.EvKey (V.KChar 'w') [])) = modify $ turn2 North
handleEvent (VtyEvent (V.EvKey (V.KChar 's') [])) = modify $ turn2 South
handleEvent (VtyEvent (V.EvKey (V.KChar 'd') [])) = modify $ turn2 East 
handleEvent (VtyEvent (V.EvKey (V.KChar 'a') [])) = modify $ turn2 West 
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = do {g <- get; g' <- liftIO $ initGame $ g ^. p2mode; put g'; return ()}
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar '.') [])) = modify $ applyReverseEffect 1
handleEvent (VtyEvent (V.EvKey (V.KChar 'g') [])) = modify $ applyReverseEffect 2
handleEvent _                                     = return ()

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  if g ^. p2mode then
    [ C.center $ padRight (Pad 2)  ((drawHelp g) <=> (drawTip g) <=> padLeft (Pad 7) (drawStats g)) <+> drawGrid g]
  else
    [ C.center $ padRight (Pad 2)  ((drawHelp g) <=> (drawTip g) <=> padLeft (Pad 5) (drawStats g)) <+> drawGrid g]

drawStats :: Game -> Widget Name
drawStats g = if g ^. p2mode then 
  hLimit 11
  $ vBox [ drawScore (g ^. score1) "p1Score"
         , drawScore (g ^. score2) "p2Score"
         , padTop (Pad 2) $ drawGameOver (g ^. dead) (g ^. paused) (g ^. winner)
         ]
  else
    hLimit 11
  $ vBox [ drawScore (g ^. score1) "Score"
         , padTop (Pad 2) $ drawGameOver (g ^. dead) (g ^. paused) (g ^. winner)
         ]


drawScore :: Int -> String -> Widget Name
drawScore n s = withBorderStyle BS.unicodeRounded
  $ B.borderWithLabel (str s)
  $ C.hCenter
  $ padAll 1
  $ str $ show n

drawGameOver :: Bool -> Bool -> String -> Widget Name
drawGameOver isDead isPause winnerStr =
  if isDead
     then withAttr gameOverAttr $ C.hCenter $ str $ winnerStr
     else if isPause
      then withAttr gameOverAttr $ C.hCenter $ str $ "GAME PAUSED\nPRESS A KEY\nTO CONTINUE"
      else emptyWidget

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Snake")
  $ vBox rows
  where
    rows         = [hBox $ cellsInRow r | r <- [height-1,height-2..0]]
    cellsInRow y = [drawCoord (V2 x y) | x <- [0..width-1]]
    drawCoord    = drawCell . cellAt
    cellAt c
      | c `elem` g ^. snake1 && False == g ^. dead1 && isItemFirst c (g ^. snake1) = SnakeHead1
      | c `elem` g ^. snake1 && False == g ^. dead1                                = Snake1
      | c `elem` g ^. snake2 && False == g ^. dead2 && isItemFirst c (g ^. snake2) = SnakeHead2
      | c `elem` g ^. snake2 && False == g ^. dead2                                = Snake2
      | c == g ^. food                                                             = Food
      | otherwise                                                                  = Empty

drawHelp :: Game -> Widget()
drawHelp g = if g ^. p2mode then
    [ "Player1 Move    : ↑←↓→"
    , "Player2 Move    : WASD"
    , "Player1 Reverse : ."
    , "Player2 Reverse : g"
    , "Quit            : Q"
    , "Restart         : R"
    , "Pause           : P"
    ]
    & unlines
    & str
    & borderWithLabel (str " Help ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)
  else
    [ "Move        : ↑←↓→"
    , "Reverse     : ."
    , "Quit        : Q"
    , "Restart     : R"
    , "Pause       : P"
    ]
    & unlines
    & str
    & borderWithLabel (str " Help ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)

drawTip :: Game -> Widget()
drawTip g = if g ^. p2mode then
      [ "RED DOT IS PLAYER 1"
      , "BLUE DOT IS PLAYER 2"
      , "GREEN DOT IS FOOD"
    ]
    & unlines
    & str
    & borderWithLabel (str " Tip ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)
  else
    [ "BLUE DOT IS PLAYER"
    , "GREEN DOT IS FOOD"
    ]
    & unlines
    & str
    & borderWithLabel (str " Tip ")
    & withBorderStyle unicodeBold
    & setAvailableSize (100, 50)

drawCell :: Cell -> Widget Name
drawCell SnakeHead1 = withAttr snakeAttr1 cw1
drawCell SnakeHead2 = withAttr snakeAttr2 cw1
drawCell Snake1 = withAttr snakeAttr1 cw
drawCell Snake2 = withAttr snakeAttr2 cw
drawCell Food   = withAttr foodAttr cw
drawCell Empty  = withAttr emptyAttr cw

cw :: Widget Name
cw = str "  "

cw1 :: Widget Name
cw1 = str "**"

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (snakeAttr1, V.blue `on` V.blue)
  , (snakeAttr2, V.red `on` V.red)
  , (snakeheadAttr1, V.blue `on` V.blue)
  , (snakeheadAttr2, V.red `on` V.red)
  , (foodAttr, V.green `on` V.green)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"

snakeAttr1 :: AttrName
snakeAttr1 = attrName "snake1"

snakeAttr2 :: AttrName
snakeAttr2 = attrName "snake2"

snakeheadAttr1 :: AttrName
snakeheadAttr1 = attrName "snakehead1"

snakeheadAttr2 :: AttrName
snakeheadAttr2 = attrName "snakehead2"

foodAttr :: AttrName
foodAttr = attrName "food"

emptyAttr :: AttrName
emptyAttr = attrName "empty"
