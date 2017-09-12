module Hangman where

import Data.Char as Char
import Data.List as List

-- CONSTANTS

secretWord :: String
secretWord = "ghost"

alphabet :: [Char]
alphabet = ['a'..'z']

lives :: Int
lives = 8

-- LOGIC

singleCharacter :: [Char] -> Bool
singleCharacter input = length input == 1

alphabeticalCharacter :: Char -> Bool
alphabeticalCharacter inputCharacter = inputCharacter `elem` alphabet -- alternatively: Char.isLetter

letterInString :: Char -> [Char] -> Bool -- 1. has input been already guessed?; 2. is letter in secret word?
letterInString letter string = letter `elem` string

allGuesses :: [Char] -> [Char] -> [Char]
allGuesses currentGuess existingGuesses = currentGuess ++ existingGuesses

countWrongGuesses :: [Char] -> [Char] -> Int
countWrongGuesses guessedLetters hangmanWord = length (filter (\letter -> not (letterInString letter hangmanWord)) guessedLetters)

livesRemaining :: Int -> [Char] -> [Char] -> Int
livesRemaining lives guessedLetters hangmanWord = lives - (countWrongGuesses guessedLetters hangmanWord)

displayHangmanWord :: [Char] -> [Char] -> [Char]
displayHangmanWord guessedLetters hangmanWord = map (\letter -> if letterInString letter guessedLetters then letter else '_') hangmanWord

gameLost :: [Char] -> [Char] -> Bool
gameLost guessedLetters secretWord = (countWrongGuesses guessedLetters secretWord) >= (length (List.nub secretWord))

gameWon :: [Char] -> [Char] -> Bool
gameWon guessedLetters secretWord = (length (List.nub secretWord)) == (length guessedLetters - (countWrongGuesses guessedLetters secretWord))

gameInProgress :: [Char] -> [Char] -> Bool
gameInProgress guessedLetters secretWord = not (gameLost guessedLetters secretWord) && not (gameWon guessedLetters secretWord)

-- PROBLEM: inconsistent types: String v. Char.

main :: IO ()
main = do
  putStrLn "Hello and welcome to Hangman!"
  putStrLn ( "The word is: " ++ secretWord )
  putStrLn "Guess a letter!"
  input <- getLine
  putStrLn ( "You guessed: " ++ input )
  let asLowerCase = map Char.toLower input
  putStrLn ( "in lower case, that's: " ++ asLowerCase)
