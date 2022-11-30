module Exercises where

import Prelude hiding (putStr)

-- 1. Redefine putStr :: String -> IO () using a list comprehension and the library function sequence_ :: [IO a] -> IO ().
putStr :: String -> IO ()
putStr s = sequence_ [putChar c | c <- s]



-- 2. Using recursion, define a version of putBoard :: Board -> IO () that displays nim boards of any size, rather than being specific to boards with just five rows of stars. Hint: first define an auxiliary function that takes the current row number as an additional argument
type Board = [Int]

putRow :: Int -> Int -> IO ()
putRow row num = do putStr $ show row ++ ": "
                    putStrLn $ concat $ replicate num "* " 

putBoard :: Board -> IO ()
putBoard xs = go xs 1
    where go []     _ = return ()
          go (x:xs) n = do putRow n x
                           go xs (n + 1)



-- 3. In a similar manner to the first exercise, redefine the generalised version of putBoard using a list comprehension and sequence_.
putBoard' :: Board -> IO ()
putBoard' xs = sequence_ [putRow r n | (r,n) <- zip [1..] xs]


-- TODO
-- 4. Define an action adder :: IO () that reads a given number of integers from the keyboard, one per line, and displays their sum. For example:
-- > adder
-- How many numbers? 5
-- 1
-- 3
-- 5
-- 7
-- 9
-- The total is 25


-- TODO
-- 5. Redefine adder using the function sequence :: [IO a] -> IO [a] that performs a list of actions and returns a list of the resulting values
adder :: IO ()
adder = undefined 



-- TODO
-- 6. Using getCh, define an action readLine :: IO String that behaves in thebsame way as getLine, except that it also permits the delete key to be usedbto remove characters. Hint: the delete character is '\DEL', and the controlbcharacter for moving the cursor back one space is '\b'.
