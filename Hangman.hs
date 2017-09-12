module Hangman (main) where
import GameLogic
import Data.List as List

-- TODO: Read in file and select a random word.

totalLives :: Int
totalLives = 8

printGameInfo :: GameState -> IO ()
printGameInfo state = do
  putStrLn "\n-----------------------------\n"
  putStrLn ("The word is: " ++ (displayHangmanWord state))
  let incorrectGuesses = wrongGuesses state
  if (length incorrectGuesses > 0) then putStrLn ("Your incorrect guesses: " ++ (List.intersperse ',' incorrectGuesses)) else putStr ""
  putStrLn ("You have " ++ show (livesRemaining state) ++ " lives remaining.")

guessResult :: GameState -> String -> IO ()
guessResult state message = do
  putStrLn message
  playHangman state

playHangman :: GameState -> IO ()
playHangman state = do
 printGameInfo state
 putStr "Guess a letter!: "
 input <- getLine
 case input of
   [guess] -> performGuess state guess
   _ -> do
     guessResult state "Please enter one character! Try again"

performGuess :: GameState -> Char -> IO ()
performGuess state guess = case guessLetter state guess of
            Invalid -> do
              guessResult state "Alphabetical characters only, please!"
            (Valid newState) -> playHangman newState
            AlreadyGuessed -> do
              guessResult state ("You already guessed " ++ show (guess) ++ "! Try again.")
            Won -> putStrLn "Congrats! You won!"
            (Lost word) -> putStrLn ("You lost! The word was: " ++ word)

main :: IO ()
main = do
  putStrLn "Hello and welcome to Hangman!"
  playHangman (createInitialState "pistachio" totalLives)
