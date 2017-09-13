module Hangman (main) where
import GameLogic
import Data.List as List
import Data.Char as Char
import Network.HTTP as HTTP
import System.Random as Random

totalLives :: Int
totalLives = 8

wordListURL :: String
wordListURL = "http://seriousorange.com/words.txt"

printGameInfo :: GameState -> IO ()
printGameInfo state = do
  putStrLn "\n••••••••••••••••••••••••••\n"
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
            (Won word) -> putStrLn ("Congrats! You won! The word was: " ++ (map Char.toUpper word))
            (Lost word) -> putStrLn ("You lost! The word was: " ++ (map Char.toUpper word))

get :: String -> IO String
get url = HTTP.simpleHTTP (getRequest url) >>= getResponseBody

getRandomWord :: [String] -> IO String
getRandomWord wordList = do
  randomNo <- Random.getStdRandom (Random.randomR (0, (length wordList) - 1))
  let secretWord = wordList !! randomNo
  if length secretWord > 6
    then return secretWord
    else getRandomWord wordList

main :: IO ()
main = do
  putStrLn "Hello and welcome to Hangman!"
  response <- get wordListURL
  secretWord <- getRandomWord (lines response)
  -- putStrLn ("The secret word is: " ++ (show secretWord))
  playHangman (createInitialState secretWord totalLives)
