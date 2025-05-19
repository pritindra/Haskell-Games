module Main where

import Data.Char (toLower)
import System.Exit (exitSuccess)
import Lexicon (randomLexiconWord)
import Hangman (gameLoop, createGame) 

main :: IO ()
main = do
  word <- randomLexiconWord
  let game = createGame word
  gameLoop game
