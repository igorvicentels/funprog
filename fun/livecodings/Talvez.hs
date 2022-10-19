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

firstThat' :: (a -> Bool) -> [a] -> Maybe a
firstThat' p = safeHead . filter p

isGoodFirstThat :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat p q []     = Nothing
isGoodFirstThat p q (x:xs) | q x = Just (p x)
                           | otherwise = isGoodFirstThat p q xs

isGoodFirstThat' :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat' p q xs = case firstThat q xs of
                            Nothing -> Nothing
                            Just x -> Just (p x)

maybeize :: (a -> b) -> Maybe a -> Maybe b
maybeize _ Nothing  = Nothing
maybeize f (Just x) = Just (f x)

maybeize' :: (a -> b) -> Maybe a -> Maybe b
maybeize' f = \x -> case x of
                        Nothing -> Nothing
                        Just y -> Just $ f y

isGoodFirstThat'' :: (a -> Bool) -> (a -> Bool) -> [a] -> Maybe Bool
isGoodFirstThat'' p q = maybeize p . firstThat' q