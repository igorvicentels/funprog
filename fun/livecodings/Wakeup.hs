module Wakeup where

cp :: [[a]] -> [[a]]
cp xs = [[x,y] | x <- head xs, y <- head (tail xs)]

inserts :: a -> [a] -> [[a]]
inserts e xs = [ insertAt e i xs | i <- [0..length xs]]

inserts' :: a -> [a] -> [[a]]
inserts' e []     = [[e]]
inserts' e (x:xs) = (e:x:xs) : map (x:) (inserts' e xs)

insertAt :: a -> Int -> [a] -> [a]
insertAt e 0 xs     = e : xs 
insertAt e n []     = error "connot insert at this posisition" 
insertAt e n (x:xs) = x : insertAt e (n - 1) xs