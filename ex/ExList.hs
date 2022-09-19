module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import Llvm (LlvmCallConvention(CC_Ghc))

-- to use a function from a qualified import
-- you need to prefix its name with its alias
-- and a dot:
-- P.head   C.toUpper   etc.
-- I import these for you to test the original functions on ghci:
-- ghci> :t C.toUpper
-- C.toUpper :: Char -> Char
-- You MUST NOT use ANY of these in your code

head :: [a] -> a
head []     = error "empty list"
head (x:_) = x

tail :: [a] -> [a]
tail []     = error "empty list"
tail (_:xs) = xs

null :: [a] -> Bool
null [] = True
null _  = False

length :: Integral i => [a] -> i
length [] = 0
length (_:xs) = 1 + length xs

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs ++ [x] 

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x ys = ys ++ [x]

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?)
infixl 5 +++

-- minimum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a
minimum []     = error "empty list"
minimum [x]    = x
minimum (x:xs) = min x (minimum xs)

-- maximum :: Ord a => [a] -> a
maximum :: Ord a => [a] -> a
maximum []     = error "empty list"
maximum [x]    = x
maximum (x:xs) = max x (maximum xs)

-- take
take :: Integral i => i -> [a] -> [a]
take n _
    | n <= 0  = []
take _ []     = []
take n (x:xs) = x : take (n - 1) xs

-- drop
drop :: Integral i => i -> [a] -> [a]
drop n xs
    | n <= 0  = xs
drop _ []     = []
drop n (x:xs) = drop (n - 1) xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ []     = []
takeWhile p (x:xs) 
    | p x       = x : takeWhile p xs
    | otherwise = []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile _ []     = []
dropWhile p (x:xs)
    | p x       = dropWhile p xs
    | otherwise = (x:xs)

-- tails
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)

-- init
init :: [a] -> [a]
init []     = error "empty list"
init [_]    = []
init (x:xs) = x : init xs

-- inits // TODO: reverse it
inits :: [a] -> [[a]]
inits xs = reverse (inits' xs)
    where
        inits' [] = [[]]
        inits' xs = xs : inits' (init xs)

-- subsequences TODO
-- subsequences :: [a] -> [[a]]
-- subsequences [] = []

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) 
    | p x       = True
    | otherwise = any p xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ []     = True
all p (x:xs)
    | p x       = all p xs
    | otherwise = False

-- and  
and :: [Bool] -> Bool
and []     = True
and (x:xs) = x && and xs

-- or
or :: [Bool] -> Bool
or []     = False
or (x:xs) = x || or xs

-- concat
concat :: [[a]] -> [a]
concat []     = []
concat (x:xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem e = any (== e)

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' e (x:xs)
    | e == x    = True
    | otherwise = elem' e xs

-- (!!) TODO check better way to erite this
(!!) :: [a] -> Int -> a
_      !! n 
    | n < 0 = error "negative index"
[]     !! _ = error "index too large"
(x:_)  !! 0 = x 
(_:xs) !! n = xs !! (n - 1)

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ []     = []
filter p (x:xs) 
    | p x       = x : filter p xs 
    | otherwise = filter p xs

-- map
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs


-- cycle 
-- TODO: check if i should make the  pattern matching explicit
cycle :: [a] -> [a]
cycle [] = error "empty list"
cycle xs = xs ++ cycle xs 

-- repeat
repeat :: a -> [a]
repeat x = x : repeat x

-- replicate
replicate :: Int -> a -> [a]
replicate n _ 
    | n <= 0 = []
replicate n x = x : replicate (n - 1) x

-- isPrefixOf
isPrefixOf :: Eq a => [a] -> [a] -> Bool
isPrefixOf []     _      = True
isPrefixOf (x:xs) []     = False
isPrefixOf (x:xs) (y:ys)
    | x == y    = isPrefixOf xs ys
    | otherwise = False

-- isInfixOf
-- isInfixOf []     _      = True
-- isInfixOf (x:xs) []     = False
-- isInfixOf (x:xs) (y:ys) 
--     | x == y    = isInfixOf xs ys
--     | otherwise 

-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome = undefined

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

