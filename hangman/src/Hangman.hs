module Hangman where

import Data.List (intersperse)
import Data.Maybe (isJust)
import Control.Monad (forever)
import System.Exit (exitSuccess)

-- === Configuration ===

maxAttempts :: Int
maxAttempts = 10

-- === Output Abstraction ===

display :: String -> IO ()
display = putStrLn

-- === Data Types ===

data GameState = GameState
  { targetWord   :: String
  , revealed     :: [Maybe Char]
  , triedLetters :: [Char]
  }

instance Show GameState where
  show (GameState _ revealedLetters guessedLetters) =
    intersperse ' ' (map displayChar revealedLetters)
    ++ "\nGuessed so far: " ++ intersperse ' ' guessedLetters

displayChar :: Maybe Char -> Char
displayChar Nothing  = '_'
displayChar (Just c) = c

-- === Initialization ===

createGame :: String -> GameState
createGame word = GameState word (replicate (length word) Nothing) []

-- === Game Logic ===

isLetterInWord :: GameState -> Char -> Bool
isLetterInWord (GameState word _ _) ch = ch `elem` word

hasBeenTried :: GameState -> Char -> Bool
hasBeenTried (GameState _ _ tried) ch = ch `elem` tried

updateGameState :: GameState -> Char -> GameState
updateGameState (GameState word revealedSoFar tried) guess =
  GameState word newRevealed (guess : tried)
  where
    newRevealed = zipWith (\w r -> if w == guess then Just w else r) word revealedSoFar

missCount :: GameState -> Int
missCount (GameState word _ tried) =
  length $ filter (`notElem` word) tried

-- === Game Flow ===

handleGuess :: GameState -> Char -> IO GameState
handleGuess state guess = do
  display ""
  display $ "You guessed " ++ [guess]
  let newState = updateGameState state guess
  case (isLetterInWord state guess, hasBeenTried state guess) of
    (_, True) -> do
      display "You've already guessed this letter!"
      return newState
    (True, _) -> do
      display "You matched a letter!"
      return newState
    (False, _) -> do
      let attemptsLeft = show $ maxAttempts - missCount newState
      display $ "Uh oh, try again! You have " ++ attemptsLeft ++ " guesses left."
      return newState

checkGameOver :: GameState -> IO ()
checkGameOver gs@(GameState word _ _) =
  if missCount gs >= maxAttempts
    then do
      display "You guessed incorrectly too many times :("
      display $ "The word was: " ++ word
      exitSuccess
    else return ()

checkVictory :: GameState -> IO ()
checkVictory (GameState _ revealedLetters _) =
  if all isJust revealedLetters
    then do
      display "You win!"
      exitSuccess
    else return ()

-- === Main Game Loop ===

gameLoop :: GameState -> IO ()
gameLoop state = forever $ do
  checkGameOver state
  checkVictory state
  display $ show state
  display "Guess a letter: "
  input <- getLine
  case input of
    [ch] -> handleGuess state ch >>= gameLoop
    _    -> display "Your guess must be a single letter."

