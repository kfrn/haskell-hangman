module GameLogic (GameState, displayHangmanWord, wrongGuesses, livesRemaining, guessLetter, GuessResult(..),  createInitialState) where
import Data.Char as Char

data GameState = GameState { guessedLetters :: [Char], hangmanWord :: String , totalLives :: Int}

inputToGuess :: String -> Maybe Char
inputToGuess [guess] | Char.isLetter guess = Just guess -- checks for 1 alphabetic character
inputToGuess _ = Nothing

letterInString :: Char -> [Char] -> Bool
letterInString letter string = (Char.toLower letter) `elem` (map Char.toLower string)

wrongGuesses :: GameState -> [Char]
wrongGuesses (GameState guesses word _) = filter (\letter -> not (letterInString letter word)) guesses

livesRemaining :: GameState -> Int
livesRemaining state = (totalLives state) - length (wrongGuesses state)

recordGuess :: GameState -> Char -> GameState
recordGuess state guess = state { guessedLetters = newGuesses }
   where newGuesses = (guessedLetters state) ++ [guess]

displayHangmanWord :: GameState -> String
displayHangmanWord (GameState guesses word _) = map displayedLetter word
  where displayedLetter letter = if letterInString letter guesses then letter else '_'

gameLost :: GameState -> Bool
gameLost state = livesRemaining state == 0

gameWon :: GameState -> Bool
gameWon (GameState guesses word _) = all (`letterInString` guesses) word

gameInProgress :: GameState -> Bool
gameInProgress state = not (gameLost state) && not (gameWon state)

data GuessResult = Invalid
           | AlreadyGuessed
           | Valid GameState
           | Won String
           | Lost String

guessLetter :: GameState -> Char -> GuessResult
guessLetter state guess | letterInString guess (guessedLetters state) = AlreadyGuessed
guessLetter state guess | Char.isLetter guess =
   if gameWon newState
   then Won (hangmanWord state)
   else if gameLost newState
     then Lost (hangmanWord state)
     else Valid newState
 where newState = recordGuess state guess
guessLetter _ _ = Invalid

createInitialState :: String -> Int -> GameState
createInitialState word totalLives = GameState [] word totalLives
