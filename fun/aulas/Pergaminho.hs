module Pergaminho where

getTwoChars :: Char -> Char -> IO ()
getTwoChars x y = 
    do putChar x
       putChar y

getThreePutFL :: IO ()
getThreePutFL = 
    do x <- getChar
       getChar
       z <- getChar
       putChar x
       putChar z 

getThreeFL :: IO (Char, Char)
getThreeFL = 
    do x <- getChar
       getChar
       z <- getChar
       return (x,z)