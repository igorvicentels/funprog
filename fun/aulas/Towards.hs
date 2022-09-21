module Towards where

import Data.Char (toUpper)

numbers :: [Int]
numbers = [1,2,3,4,8,10]

phrase :: [Char]
phrase = "Hello There"

scream :: [Char] -> [Char]
scream = map toUpper
-- scream []     = []
-- scream (x:xs) = toUpper x : scream xs

squareAll :: [Int] -> [Int]
squareAll = map (^2)
-- squareAll []     = []
-- squareAll (x:xs) = (^2) x : squareAll xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs