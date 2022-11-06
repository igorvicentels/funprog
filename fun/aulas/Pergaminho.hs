module Pergaminho where

getTwoChars :: Char -> Char -> IO ()
getTwoChars x y = 
    do putChar x
       putChar y

putThreePutFL :: IO ()
putThreePutFL = 
    do x <- getChar
       getChar
       z <- getChar
       putChar x
       putChar z 

putThreeFL :: IO (Char, Char)
putThreeFL = 
    do x <- getChar
       getChar
       z <- getChar
       return (x,z)