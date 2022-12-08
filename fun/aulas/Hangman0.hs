module Hangman0 where

import System.IO
import Data.Char

hangman :: IO ()
hangman = do putStr "Choose word: "
             word <- getSecretLine
             putStrLn "Try to guess it"
             play word

echoless :: IO a -> IO a
echoless ax = 
    do oldEcho <- hGetEcho stdin
       hSetEcho stdin False
       x <- ax
       hSetEcho stdin oldEcho
       return x

getSecretLine :: IO String
getSecretLine = echoless getLine

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