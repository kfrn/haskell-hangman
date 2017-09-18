module Main
  ( main
  ) where

import Data.Char as Char
import Data.List as List
import GameLogic
import Network.HTTP as HTTP
import System.Random as Random

totalLives :: Int
totalLives = 8

wordListURL :: String
wordListURL = "http://seriousorange.com/words.txt"

printGameInfo :: GameState -> IO ()
printGameInfo state = do
  putStrLn "\n••••••••••••••••••••••••••\n"
  putStrLn ("The word is: " ++ displayHangmanWord state)
  let incorrectGuesses = wrongGuesses state
  if null incorrectGuesses
    then putStr ""
    else putStrLn
           ("Your incorrect guesses: " ++ List.intersperse ',' incorrectGuesses)
  putStrLn ("You have " ++ show (livesRemaining state) ++ " lives remaining.")

guessResult :: GameState -> String -> IO ()
guessResult state message = do
  putStrLn message
  playHangman state

playHangman :: GameState -> IO ()
playHangman state = do
  printGameInfo state
  putStrLn "Guess a letter!: "
  input <- getLine
  case input of
    [guess] -> performGuess state guess
    _ -> guessResult state "Please enter one character! Try again"

performGuess :: GameState -> Char -> IO ()
performGuess state guess =
  case guessLetter state guess of
    Invalid -> guessResult state "Alphabetical characters only, please!"
    (Valid newState) -> playHangman newState
    AlreadyGuessed ->
      guessResult state ("You already guessed " ++ show guess ++ "! Try again.")
    (Won word) ->
      putStrLn ("Congrats! You won! The word was: " ++ map Char.toUpper word)
    (Lost word) ->
      putStrLn ("You lost! The word was: " ++ map Char.toUpper word)

get :: String -> IO String
get url = HTTP.simpleHTTP (getRequest url) >>= getResponseBody

getRandomWord :: [String] -> IO String
getRandomWord wordList
  -- let longWords = filter ((> 6) . length) wordList
 = do
  let longWords = filter (\word -> length word > 6) wordList
  randomNo <- Random.getStdRandom (Random.randomR (0, length longWords - 1))
  return (longWords !! randomNo)

main :: IO ()
main = do
  putStrLn "Hello and welcome to Hangman!"
  response <- get wordListURL
  secretWord <- getRandomWord (lines response)
  -- putStrLn ("The secret word is: " ++ (show secretWord))
  playHangman (createInitialState secretWord totalLives)
