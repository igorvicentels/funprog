module Hangman0 where

import System.IO
import Data.Char

hangman :: IO ()
hangman = do putStr "Choose word: "
             word <- getSecretLine
             putStrLn "Try to guess it"
             play word

getSecretLine :: IO String
getSecretLine = 
    do hSetEcho stdin False
       x <- getLine
       hSetEcho stdin True
       return x

prompt :: IO ()
prompt = putStr "? "

play :: String -> IO ()
play word = 
    do prompt
       guess <- getLine
       if guess == word
          then putStrLn "You got it!"
          else do putStrLn "Nope..."
                  play word