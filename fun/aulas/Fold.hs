module Fold where

import Prelude hiding ( sum
                      , product
                      , or
                      , and
                      , any
                      , all
                      , reverse
                      , minimum
                      , maximum
                      , length
                      , map
                      , filter
                      , takeWhile
                      )

badId :: [a] -> [a]
badId = foldr (:) []

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

or :: [Bool] -> Bool
or = foldr (||) False

and :: [Bool] -> Bool
and = foldr (&&) True

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x w -> p x || w) False 

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x w -> p x && w) True

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

-- (++) :: [a] -> [a] -> [a]
-- (++) xs = foldr ()

minimum :: Ord a => [a] -> a
minimum = foldr1 min

maximum :: Ord a => [a] -> a
maximum = foldr1 max

length :: [a] -> Int
length = foldr (\x w -> 1 + w) 0

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x w -> f x : w) []

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x w -> if p x then x : w else w) []

inits :: [a] -> [[a]]
inits = foldr f v
    where v = [[]]
          f x yss = [] : map (x:) yss

-- inits [1,2,3] =
-- [[],[1],[1,2],[1,2,3]]

-- inits [2,3] =
-- [[],[2],[2,3]]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr f v
    where v     = []
          f x w = if p x then x : w else [] 
    