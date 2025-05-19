module Lexicon where

import System.Random (randomRIO)

newtype Lexicon = Lexicon [String]
  deriving (Eq, Show)

-- Default dictionary file path
lexiconFilePath :: FilePath
lexiconFilePath = "src/dict"

-- Reads the lexicon file and returns a Lexicon
loadLexicon :: FilePath -> IO Lexicon
loadLexicon filePath = do
  fileContent <- readFile filePath
  return $ Lexicon (lines fileContent)

-- Predicate to check if a word is acceptable for the game
isAcceptableWord :: String -> Bool
isAcceptableWord word = length word > minLen && not (containsApostrophe word) && not (containsBackslash word)
  where
    minLen = 5
    containsApostrophe = elem '\''
    containsBackslash = elem '\\'

-- Filters a Lexicon to only include acceptable words
filterLexicon :: Lexicon -> Lexicon
filterLexicon (Lexicon wordsList) = Lexicon $ filter isAcceptableWord wordsList

-- Gets the list of acceptable game words from a lexicon file
fetchGameWords :: FilePath -> IO Lexicon
fetchGameWords filePath = do
  lexicon <- loadLexicon filePath
  return $ filterLexicon lexicon

-- Selects a random word from a Lexicon
selectRandomWord :: Lexicon -> IO String
selectRandomWord (Lexicon wordsList) = do
  randomIndex <- randomRIO (0, length wordsList - 1)
  return $ wordsList !! randomIndex

-- Gets a random acceptable game word from a lexicon file
fetchRandomGameWord :: FilePath -> IO String
fetchRandomGameWord filePath = do
  gameWords <- fetchGameWords filePath
  selectRandomWord gameWords

-- Convenience function: gets a random word from the default lexicon
randomLexiconWord :: IO String
randomLexiconWord = fetchRandomGameWord lexiconFilePath

