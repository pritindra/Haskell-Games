{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Hangman (runHangman) where

import Brick
import Brick.Widgets.Border
import Brick.Widgets.Center
import Brick.Widgets.Edit
import qualified Graphics.Vty as V
import Data.List (intersperse)
import Data.Maybe (isJust)
import Lens.Micro.TH (makeLenses)
import Lens.Micro ((^.), (&), (.~), (%~))
import Control.Monad.IO.Class (liftIO)

-- === Configuration ===

maxAttempts :: Int
maxAttempts = 10

-- === Data Types ===

data Name = InputField deriving (Ord, Show, Eq)

data GameState = GameState
  { _targetWord   :: String
  , _revealed     :: [Maybe Char]
  , _triedLetters :: [Char]
  , _message      :: String
  , _editorState  :: Editor String Name
  }

makeLenses ''GameState

-- === Initialization ===

createGame :: String -> GameState
createGame word = GameState word (replicate (length word) Nothing) [] "" (editor InputField (Just 1) "")

-- === Game Logic ===

isLetterInWord :: GameState -> Char -> Bool
isLetterInWord gs ch = ch `elem` (gs ^. targetWord)

hasBeenTried :: GameState -> Char -> Bool
hasBeenTried gs ch = ch `elem` (gs ^. triedLetters)

updateGameState :: GameState -> Char -> GameState
updateGameState gs guess =
  gs & revealed %~ zipWith (\w r -> if w == guess then Just w else r) (gs ^. targetWord)
     & triedLetters %~ (guess :)

missCount :: GameState -> Int
missCount gs = length $ filter (`notElem` (gs ^. targetWord)) (gs ^. triedLetters)

-- === Brick App Definition ===

app :: App GameState () Name
app = App
  { appDraw = drawUI
  , appChooseCursor = showFirstCursor
  , appHandleEvent = handleEvent
  , appStartEvent = pure ()
  , appAttrMap = const theMap
  }

-- === UI Drawing ===

drawUI :: GameState -> [Widget Name]
drawUI gs =
  [center $ border $ vBox
    [ str "Hangman Game"
    , padTop (Pad 1) $ str $ "Word: " ++ displayWord (gs ^. revealed)
    , padTop (Pad 1) $ str $ "Guessed letters: " ++ intersperse ' ' (gs ^. triedLetters)
    , padTop (Pad 1) $ str $ "Misses left: " ++ show (maxAttempts - missCount gs)
    , padTop (Pad 1) $ str $ gs ^. message
    , padTop (Pad 1) $ str "Type a letter and press Enter to guess"
    , padTop (Pad 1) $ renderEditor (str . unlines) True (gs ^. editorState)
    ]
  ]

displayWord :: [Maybe Char] -> String
displayWord = intersperse ' ' . map displayChar

displayChar :: Maybe Char -> Char
displayChar Nothing = '_'
displayChar (Just c) = c

-- === Event Handling ===

handleEvent :: BrickEvent Name () -> EventM Name GameState ()
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  gs <- get
  let inputStr = concat $ getEditContents (gs ^. editorState)
  case inputStr of
    [c] | c `elem` ['a'..'z'] || c `elem` ['A'..'Z'] -> do
      let newState = processGuess gs c
      if isGameOver newState || isVictory newState
        then do
          put newState
          liftIO $ putStrLn $ if isGameOver newState 
                             then "Game over! The word was: " ++ (newState ^. targetWord)
                             else "You win!"
          halt
        else put $ clearEditor newState
    _ -> put $ gs & message .~ "Your guess must be a single letter."
handleEvent (VtyEvent (V.EvKey (V.KChar c) [])) = do
  gs <- get
  put $ gs & editorState %~ insertChar c & message .~ ""
handleEvent (VtyEvent (V.EvKey V.KBS [])) = do
  gs <- get
  put $ gs & editorState %~ deleteChar & message .~ ""
handleEvent _ = return ()

-- Helper to insert a character into the editor
insertChar :: Char -> Editor String Name -> Editor String Name
insertChar c ed = editor InputField (Just 1) (concat (getEditContents ed) ++ [c])

-- Helper to delete the last character from the editor
deleteChar :: Editor String Name -> Editor String Name
deleteChar ed = 
  let content = concat (getEditContents ed)
      newContent = if null content then "" else init content
  in editor InputField (Just 1) newContent

clearEditor :: GameState -> GameState
clearEditor gs = gs & editorState .~ editor InputField (Just 1) ""

-- === Game Logic Helpers ===

processGuess :: GameState -> Char -> GameState
processGuess gs guess
  | hasBeenTried gs guess = gs & message .~ "You've already guessed this letter!"
  | isLetterInWord gs guess = updateGameState gs guess & message .~ "You matched a letter!"
  | otherwise = updateGameState gs guess & message .~ "Uh oh, try again!"

isGameOver :: GameState -> Bool
isGameOver gs = missCount gs >= maxAttempts

isVictory :: GameState -> Bool
isVictory gs = all isJust (gs ^. revealed)

-- === Attribute Map ===

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (editAttr, V.white `on` V.blue)
  , (editFocusedAttr, V.black `on` V.yellow)
  ]

-- === Main function to run the app ===

runHangman :: String -> IO ()
runHangman word = do
  let initialState = createGame word
  _ <- defaultMain app initialState
  return ()

