module Exercises where

import Data.Char (toUpper, toLower, isLower, isAlpha)

-- Exercise A. 
-- On the subject of precedence, this question comes from Chris Maslanka’s puzzle page in the Guardian newspaper:
-- ‘Is a half of two plus two equal to two or three?’

-- Answer: Yes


-- Exercise B. 
-- Some of the following expressions are not syntactically correct, while others are syntactically correct but do not have sensible types. Some are well-formed. Which is which? In the case of a well-formed expression, give a suitable type. Assume double :: Int -> Int. I suggest you don’t use a computer to check your answers, but if you do, be prepared for some strange error messages.
-- The expressions are:

-- [0,1)                      parse error
-- double -3                  type error
-- double (-3)                Int
-- double double 0            type error
-- if 1==0 then 2==1          parse error
-- "++" == "+" ++ "+"         Bool
-- [(+),(-)]                  Num a => [a -> a -> a]
-- [[],[[]],[[[]]]]           [[[[a]]]]
-- concat ["tea","for",'2']   type error
-- concat ["tea","for","2"]   [Char]


-- Exercise C. 
-- In the good old days, one could write papers with titles such as ‘The morphology of prex – an essay in meta-algorithmics’ These days, journals seem to want all words capitalised:
-- ‘The Morphology Of Prex – An Essay In Meta-algorithmics’
-- Write a function modernise :: String -> String which ensures that paper titles are capitalised as above. Here are some helpful questions to answer first:
-- 1. The function toLower :: Char -> Char converts a letter to lowercase. What do you think is the name of the prelude function that converts a letter to upper-case?
-- 2. The function words :: String -> [Word] was used in the previous chapter.What do you think the prelude function unwords :: [Word] -> String does? Hint: which, if either, of the following equations should hold?
-- words . unwords = id
-- unwords . words = id
-- 3. The function head :: [a] -> a returns the head of a nonempty list, and tail :: [a] -> [a] returns the list that remains when the head is removed. Suppose a list has head x and tail xs. How would you reconstruct the list?

capitalise :: [Char] -> [Char]
capitalise []     = []
capitalise (x:xs) = toUpper x : xs 

modernise :: [Char] -> [Char]
modernise = unwords . map capitalise . words



-- Exercise D. 
-- Beaver is an eager evaluator, while Susan is a lazy one.
-- How many times would Beaver evaluate f in computing head (map f xs) when xs is a list of length n? How many times would Susan? What alternative to head . map f would Beaver prefer?
-- The function filter p filters a list, retaining only those elements that satisfy the boolean test p. The type of filter is filter :: (a -> Bool) -> [a] -> [a]
-- Susan would happily use head . filter p for a function that finds the first element of a list satisfying p. Why would Beaver not use the same expression?
-- Instead, Beaver would probably define something like 
-- first :: (a -> Bool) -> [a] -> a
-- first p xs | null xs   = error "Empty list"
--            | p x       = ...
--            | otherwise = ...
--            where x = head xs
-- The function null returns True on an empty list, and False otherwise. When evaluated, the expression error message stops execution and prints the string message at the terminal, so its value is ⊥. Complete the right-hand side of Beaver’s definition.
-- What alternative might Beaver prefer to head . filter p . map f?

-- Answer: 
-- In computing head (map f xs), Beaver would evaluate f n times, susan would do it once. Beaver would prefer f (head xs).
-- Because the strict evaluation would make beaver filter the whole list and then taking the head of the resulting list. Susan would just filter till it find the first element x for wich p x holds.
first :: (a -> Bool) -> [a] -> a
first p xs | null xs   = error "Empty list"
           | p x       = x
           | otherwise = first p (tail xs)
           where x = head xs

firstMap :: (b -> Bool) -> (a -> b) -> [a] -> b
firstMap p f xs | null xs   = error "empty list"
                | p (f x)   = f x
                | otherwise = firstMap p f (tail xs)
                where x = head xs



-- Exercise E.

-- Answer
first' :: (a -> Bool) -> [a] -> Maybe a
first' _ []     = Nothing
first' p (x:xs) 
    | p x       = Just x
    | otherwise = first' p xs



-- Exercise F.
-- Here is a function for computing x to the power n, where n ≥ 0:
-- exp :: Integer -> Integer -> Integer
-- exp x n | n == 0    = 1
--         | n == 1    = x
--         | otherwise = x * exp x (n-1)
-- How many multiplications does it take to evaluate exp x n?
-- Dick, a clever programmer, claims he can compute exp x n with far fewer multiplications:
-- exp x n | n == 0 = 1
--         | n == 1 = x
--         | even n = ...
--         | odd n = ...
-- Fill in the dots and say how many multiplications it takes to evaluate the expression exp x n by Dick’s method, assuming 2p ≤ n < 2p+1.

-- Answer:
-- It takes n-1 multiplications to evaluate exp x n. Dick’s method is to exploit the identities x^(2m) = (x^2)^m and x^(2m+1) = x(x^2)^m to obtain a recursive definition:
exp' :: Integer -> Integer -> Integer
exp' x n | n == 0 = 1
         | n == 1 = x
         | even n = exp' (x * x) m 
         | odd n  = x * exp' (x * x) (m)
         where m = n `div` 2



-- Exercise G.
-- Suppose a date is represented by three integers (day,month,year). Define a function showDate :: Date -> String so that, for example,
-- showDate (10,12,2013) = "10th December, 2013"
-- showDate (21,11,2020) = "21st November, 2020"
-- You need to know that Int is a member of the type class Show, so that show n produces a string that is the decimal representation of the integer n.

-- Answer:
type Date = (Integer, Integer, Integer)

showDate :: Date -> String
showDate (d, m, y) = show d ++ suffix d ++ " " ++ (months !! (fromIntegral (m - 1))) ++ ", " ++ show y

suffix d | d == 1 || d == 21 || d == 31 = "st"
         | d == 2 || d == 22            = "nd"
         | d == 3 || d == 23            = "rd"
         | otherwise                    = "th"

months = ["January", "Febuary", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]



-- Exercise H.

-- Answer
-- TODO: check how to add restrictions to types
type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum xs = xs ++ (show $ addSum' xs)
    where
        addSum' ""     = 0
        addSum' (x:xs) = getDigit x + addSum' xs

valid :: CIN -> Bool
valid xs = addSum (take 8 xs) == xs



-- Exercise I.

-- Answer 
palindrome :: IO ()
palindrome
    = do {putStrLn "Enter a string:";
          xs <- getLine;
          if isPalindrome xs
            then putStrLn "Yes!"
            else putStrLn "No!";}

isPalindrome :: String -> Bool
isPalindrome xs = xs' == reverse xs'
    where xs' = (map toLower . filter isAlpha) xs