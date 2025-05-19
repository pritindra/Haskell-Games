module Main where

import Data.Char (toLower)
import System.Exit (exitSuccess)
import Lexicon (randomLexiconWord)
import Hangman (runHangman) 

main :: IO ()
main = do
  word <- randomLexiconWord
  runHangman word
