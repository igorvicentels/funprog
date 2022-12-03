module Ex02 where

-- 2. Suppose that a coordinate grid of size m x n is given by the list of all pairs (x,y) of integers such that 0 <= x <= m and 0 <= y <= n. Using a list comprehension, define a function grid :: Int -> Int -> [(Int,Int)] that returns a coordinate grid of a given size. For example:
-- > grid 1 2
-- [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]

grid :: Int -> Int -> [(Int, Int)]
grid n m = [(x,y) | x <- [0..n], y <- [0..m]]