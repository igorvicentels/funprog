module Wakeup where

cp :: [[a]] -> [[a]]
cp xs = [[x,y] | x <- head xs, y <- head (tail xs)]

inserts :: a -> [a] -> [[a]]
inserts e xs = take (length xs + 1) (inserts' e 0 xs)
    where 
        inserts' e _ [] = []
        inserts' e n xs = insertAt e n xs : inserts' e (n + 1) xs

insertAt :: a -> Int -> [a] -> [a]
insertAt e 0 xs = e : xs 
insertAt e i (x:xs) = x : insertAt e (i - 1) xs