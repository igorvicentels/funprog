module ExList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , curry , uncurry
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
reverse (x:xs) = reverse xs <: x 

(++) :: [a] -> [a] -> [a]
[]     ++ ys = ys
(x:xs) ++ ys = x : xs ++ ys

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc v []     = [v]
snoc v (x:xs) = x : snoc v xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

flip :: (a -> b -> c) -> b -> a -> c
flip f x y = f y x 

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
drop n (_:xs) = drop (n - 1) xs

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
-- inits [] = [[]]
-- inits (x:xs) = [] : map (x:) (inits xs)

-- subsequences TODO
subsequences :: [a] -> [[a]]
subsequences []     = []
subsequences [x]    = [[],[x]]
subsequences (x:xs) = subsequences xs ++ map (x:) (subsequences xs)

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ []     = False
any p (x:xs) 
    | p x       = True
    | otherwise = any p xs

-- TODO: do it in 2 lines
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

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = concat . map (\x -> if p x then [x] else [])

-- map
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs


-- cycle 
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
isInfixOf :: Eq a => [a] -> [a] -> Bool
isInfixOf [] []     = True
isInfixOf xs []     = False
isInfixOf xs (y:ys)
    | xs == take (length xs) (y:ys) = True
    | otherwise                     = isInfixOf xs ys 

-- isSuffixOf
isSuffixOf :: Eq a => [a] -> [a] -> Bool
isSuffixOf xs ys = xs == (drop (length ys - length xs) ys)

-- zip
zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys 

-- zipWith
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith _ []     _      = []
zipWith _ _      []     = []
zipWith f (x:xs) (y:ys) = f x y : zipWith f xs ys

-- intercalate
intercalate :: [a] -> [[a]] -> [a]
intercalate _ []     = []
intercalate _ [x]    = x
intercalate s (x:xs) = x ++ s ++ (intercalate s xs)

-- nub
nub :: Eq a => [a] -> [a]
nub []     = []
nub (x:xs) = x : nub (filter (/= x) xs)  

-- splitAt
splitAt :: Int -> [a] -> ([a], [a])
splitAt n xs 
    | n <= 0 = ([], xs)
splitAt n []     = ([], [])
splitAt n (x:xs) = (x : ls, rs)
    where (ls, rs) = splitAt (n - 1) xs

-- TODO
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break
break :: (a -> Bool) -> [a] -> ([a], [a])
break _ []     = ([], [])
break p (x:xs) 
    | p x       = ([], x : xs)
    | otherwise = (x : ls, rs)
    where (ls, rs) = break p xs

split :: Eq a => a -> [a] -> [[a]]
split s []     = [] 
split s (x:xs) 
    | x == s    = h : split s t
    | otherwise = (x : h) : split s t
    where
        (h, t) = break (== s) xs

-- lines
lines :: [Char] -> [[Char]]
lines = split '\n'

-- words
words :: [Char] -> [[Char]]
words = split ' '

-- unlines
unlines :: [String] -> String
unlines []     = ""
unlines (l:ls) = l ++ "\n" ++ unlines ls

-- unwords
unwords :: [String] -> String
unwords []     = ""
unwords [w]    = w
unwords (w:ws) = w ++ " " ++ unwords ws

-- transpose
-- transpose :: [[a]] -> [[a]]
-- transpose [] = []
-- transpose xs = map head xs : transpose (map tail xs) 

-- checks if the letters of a phrase form a palindrome (see below for examples)
palindrome :: String -> Bool
palindrome cs = palindrome' $ (map C.toLower . filter C.isAlpha) cs
    where palindrome' ""     = True
          palindrome' [_]    = True
          palindrome' (c:cs) = if c == L.last cs
                               then palindrome (init cs)
                               else False

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}

