module Exercises where

import Data.Char (toUpper, isLower)

-- A. On the subject of precedence, this question comes from Chris Maslanka’s puzzle page in the Guardian newspaper:
-- ‘Is a half of two plus two equal to two or three?’

-- Answer: Yes


-- B. Some of the following expressions are not syntactically correct, while others are syntactically correct but do not have sensible types. Some are well-formed. Which is which? In the case of a well-formed expression, give a suitable type. Assume double :: Int -> Int. I suggest you don’t use a computer to check your answers, but if you do, be prepared for some strange error messages.
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


-- C. In the good old days, one could write papers with titles such as ‘The morphology of prex – an essay in meta-algorithmics’ These days, journals seem to want all words capitalised:
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