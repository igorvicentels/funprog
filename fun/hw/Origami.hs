module Origami where

import Prelude hiding
    ( foldl , foldl1 , foldr , foldr1
    , scanl, scanr
    , sum , product
    , length
    , concat
    , filter
    , map
    , any , all
    , and , or
    , takeWhile , dropWhile
    , minimum, maximum
    )

import qualified Prelude as P

--
-- define the following folds:
--

-- foldr (#) v [x1, x2, x3, x4] = (x1 # (x2 # (x3 # (x4 # v))))
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f v []     = v
foldr f v (x:xs) = f x (foldr f v xs)

-- foldl (#) v [x1, x2, x3, x4] = ((((v # x1) # x2) # x3) # x4)
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f v []     = v
foldl f v (x:xs) = foldl f (f v x) xs

-- foldr1 (#) [x1, x2, x3, x4] = (x1 # (x2 # (x3 # x4)))
foldr1 :: (a -> a -> a) -> [a] -> a
foldr1 f []     = error "empty list"
foldr1 f [x]    = x 
foldr1 f (x:xs) = f x (foldr1 f xs)   

-- foldl1 (#) [x1, x2, x3, x4]  = (((x1 # x2) # x3) # x4)
foldl1 :: (a -> a -> a) -> [a] -> a
foldl1 f []     = error "empty list"
foldl1 f [x]    = x
foldl1 f (x:xs) = foldl f x xs

--
-- define the following scans:
-- (scans are like folds but return all intermediate calculations)
--
-- foldl (+) 0 [12,25,16,24] = ((((0 + 12) + 25) + 16) + 24)
-- scanl (+) 0 [12,25,16,24] = [   0 , 12  , 37  , 53  , 77]
--
-- foldr (+) 0 [12,25,16,24] = (12 + (25 + (16 + (24 + 0))))
-- scanr (+) 0 [12,25,16,24] = [77 ,  65 ,  40 ,  24 , 0   ]
--

scanl :: (b -> a -> b) -> b -> [a] -> [b]
scanl f v []     = undefined

scanr :: (a -> b -> b) -> b -> [a] -> [b]
scanr f v []     = [v]
scanr f v (x:xs) = f x (foldr f v xs) : scanr f v xs 

--
-- Define all of the following functions as folds:
--

sum :: Num a => [a] -> a
sum = foldr (+) 0

product :: Num a => [a] -> a
product = foldr (*) 1

concat :: [[a]] -> [a]
concat = foldr (++) []

any :: (a -> Bool) -> [a] -> Bool
any p = foldr (\x acc -> p x || acc) False

all :: (a -> Bool) -> [a] -> Bool
all p = foldr (\x acc -> p x && acc) True

and :: [Bool] -> Bool
and = foldr (&&) True

or :: [Bool] -> Bool
or = foldr (||) False

minimum :: Ord a => [a] -> a
minimum = foldr1 min

maximum :: Ord a => [a] -> a
maximum = foldr1 max

length :: Integral i => [a] -> i
length = foldr (\_ acc -> 1 + acc) 0

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\x acc -> if p x then x : acc else acc) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x acc -> f x : acc) []

reverse :: [a] -> [a]
reverse = foldl (flip (:)) []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p = foldr (\x acc -> if p x then x : acc else []) []

dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile = undefined

-- sum of evens, safeMaximum of odds
-- e.g.:
-- semo [1..10] = (30, Just 9)
-- semo [2,4,6] = (12, Nothing)
semo :: Integral i => [i] -> (i, Maybe i)
semo xs = ((sum . filter P.even) xs, (safeMaximum . filter P.odd) xs)
    where safeMaximum []     = Nothing
          safeMaximum (x:xs) = Just $ maximum (x:xs)
 
-- removes adjacent duplicates
-- e.g.:
-- remdups [1,2,2,3,3,3,1,1] = [1,2,3,1]
remdups :: Eq a => [a] -> [a]
remdups = undefined

safeLast :: [a] -> Maybe a
safeLast = undefined

-- dec2int [1,9,9,2] = 1992
dec2int :: Integral i => [i] -> i
dec2int = undefined

