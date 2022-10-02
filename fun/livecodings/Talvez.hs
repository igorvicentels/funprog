module Talvez where

import Prelude hiding ( Maybe(..) )

data Maybe a = Nothing
             | Just a
            deriving ( Show, Eq )

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

firstThat :: (a -> Bool) -> [a] -> Maybe a
firstThat _ []     = Nothing
firstThat p (x:xs) | p x       = Just x
                   | otherwise = firstThat p xs

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat p q []     = Nothing
isGoodFirstThat p q (x:xs) | q x = Just (p x)
                           | otherwise = isGoodFirstThat p q xs
