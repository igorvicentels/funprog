module IOwarmup where

import Prelude hiding
    ( putStr
    , putStrLn
    , getLine
    )

-- what is the type?  (don't cheat)
whatIsMyType =
    do v1 <- getChar
       v2 <- getChar
       putStrLn $ "\nI have all I need:" ++ [v1,v2]
       return [v1,v2]

putStr :: String -> IO ()
putStr xs = 
    do case xs of 
        []     -> return ()
        (y:ys) -> do putChar y
                     putStr ys
    

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'

getLine :: IO String
getLine = do
    c <- getChar
    if c == '\n'
        then return ""
        else do l <- getLine
                return (c : l)

putNtimes :: Integral i => i -> Char -> IO ()
putNtimes n c = do if n == 0
                    then return ()
                    else do putChar c
                            putNtimes (n - 1) c

doNtimes :: Integral i => i -> IO a -> IO [a]
doNtimes 0 _  = return []
doNtimes n ax = do x <- ax
                   xs <- doNtimes (n-1) ax
                   return (x:xs)                

doForever :: IO a -> IO ()
doForever ax = do ax
                  doForever ax
                  

when :: Bool -> IO () -> IO ()
when False _  = return ()
when True  ax = ax

-- consult read.txt to learn about read
getInteger :: IO Integer
getInteger = do l <- getLine
                return (read l :: Integer)

