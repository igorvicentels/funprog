module Pref where

import Prelude hiding 
    ( sum
    , product
    , length
    , concat
    , filter
    , map
    , any
    , all
    , and
    , or
    , foldr
    )

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr op b []     = b
foldr op b (x:xs) = x `op` (foldr op b xs)

sum' = foldr (+) 0

product' = foldr (*) 1

length' = foldr (\_ -> \y -> 1 + y) 0

concat' = foldr (++) []

filter' p = foldr (\x -> \y -> if p x then x : y else y) []

map' f = foldr (\x -> \y -> f x : y) []

any' p = foldr (\x -> \y -> p x || y) False

all' p = foldr (\x -> \y -> p x && y) True

and' = foldr (&&) True

or' = foldr (||) False

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

length :: Integral i => [a] -> i
length []     = 0
length (x:xs) = 1 + length xs

concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) = if p x 
                    then x : filter p xs 
                    else filter p xs

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) = p x || any p xs 

all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs) = p x && all p xs

and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

or :: [Bool] -> Bool
or [] = False
or (x:xs) = x || or xs