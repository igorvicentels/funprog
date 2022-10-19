module Exercises where

-- Exercise A.
-- Which of the following equations are true for all xs and which are false?
-- []:xs = xs               - False
-- []:xs = [[],xs]          - True 
-- xs:[] = xs               - False
-- xs:[] = [xs]             - True
-- xs:xs = [xs,xs]          - False
-- [[]] ++ xs = xs          - False
-- [[]] ++ xs = [[],xs]     - False
-- [[]] ++ [xs] = [[],xs]   - True
-- [xs] ++ [] = [xs]        - True

-- By the way, why didn’t we define null = (==[])?
-- Because using pattern matching is better



-- Exercise B
-- You want to produce an infinite list of all distinct pairs (x,y) of natural numbers. It doesn’t matter in which order the pairs are enumerated, as long as they all are there. Say whether or not the definition
-- allPairs = [(x,y) | x <- [0..], y <- [0..]]
-- does the job. If you think it doesn’t, can you give a version that does?

-- Answer: It does not work as expected because x will always be 0
allPairs = [(x,y) | y <- [0..], x <- [0..y]]



-- Exercise C
-- Give a definition of the function
-- disjoint :: (Ord a) => [a] -> [a] -> Bool
-- that takes two lists in ascending order, and determines whether or not they have an element in common.

-- Answer:
disjoint :: (Ord a) => [a] -> [a] -> Bool
disjoint xs'@(x:xs) ys'@(y:ys) 
    | x == y    = True
    | x < y     = disjoint xs ys'
    | otherwise = disjoint xs' ys
disjoint _          _          = False



-- TODO
-- Exercise D
-- Under what conditions do the following two list comprehensions deliver the same result?
-- [e | x <- xs, p x, y <- ys]
-- [e | x <- xs, y <- ys, p x]
-- Compare the costs of evaluating the two expressions



-- Exercise E
-- When the great Indian mathematician Srinivasan Ramanujan was ill in a London hospital, he was visited by the English mathematician G.H. Hardy. Trying to find a subject of conversation, Hardy remarked that he had arrived in a taxi with the nummber 1729, a rather boring number it seemed to him. Not at all, Ramanujan instantlyreplied, it is the first number that can be expressed as two cubes in essentially different ways: 1^3 +12^3 = 9^3 +10^3 = 1729. Write a program to find the second such number.
-- In fact, define a function that returns a list of all essentially different quadruples(a,b,c,d) in the range 0 < a,b,c,d ≤ n such that a^3 +b^3 = c^3 + d^3. I suggest using a list comprehension, but only after thinking carefully about what it means to say two quadruples are essentially different. After all, a^3 + b^3 = c^3 +d^3 can be writtenin eight different ways.

listOfQuads :: Int -> [(Int,Int,Int,Int)]
listOfQuads n = [(x,y,z,w) | x <- [1..n], y <- [x..n], z <- [x + 1..n], w <- [z..n], x ^ 3 + y ^ 3 == z ^ 3 + w ^ 3]



-- Exercise F
-- The dual view of lists is to construct them by adding elements to the end of the list:
data List a = Nil | Snoc (List a) a
    deriving (Show)
-- Snoc is, of course, Cons backwards. With this view of lists [1,2,3] would be represented by
-- Snoc (Snoc (Snoc Nil 1) 2) 3
-- Exactly the same information is provided by the two views but it is organised differently. Give the definitions of head and last for the snoc-view of lists, and definetwo functions
-- toList :: [a] -> List a
-- fromList :: List a -> [a]
-- for converting efficiently from one view of lists to the other. (Hint: reverse is efficient, taking linear time to reverse a list.)

last' :: List a -> a
last' Nil         = error "empty list"
last' (Snoc xs x) = x

head' :: List a -> a
head' Nil          = error "empty list"
head' (Snoc Nil x) = x
head' (Snoc xs x)  = head' xs

toList :: [a] -> List a
toList = convert . reverse
    where
        convert []     = Nil
        convert (x:xs) = (Snoc (convert xs) x)

fromList :: List a -> [a]
fromList = reverse . convert
    where 
        convert Nil         = []
        convert (Snoc ys y) = y : (convert ys) 



-- TODO
-- Exercise G
-- How much space is required to evaluate length xs? Consider the following alternative definition of length:
-- length :: [a] -> Int
-- length xs = loop (0,xs)
--     where 
--         loop (n,[]) = n
--         loop (n,x:xs) = loop (n+1,xs)
-- Does the space requirement change? Does it change if we switched to eager evaluation? These questions are taken up in much more detail in Chapter 7.



-- Exercise H
-- The prelude function take n takes the first n elements of a list, while drop n drops the first n elements. Give recursive definitions for these functions. What are the values of
-- take 0 undefined
-- take undefined []
-- according to your definition? A more tricky question: can you find a definition in which both the above expressions have the value []? If not, why not? 
-- Which of the following equations are valid for all integers m and n? You don’t have to justify your answers, just try to understand what they claim to say.
-- take n xs ++ drop n xs = xs
-- take m . drop n        = drop n . take (m+n)
-- take m . take n        = take (m `min` n)
-- drop m . drop n        = drop (m+n)
-- The standard prelude function splitAt n can be defined by
-- splitAt n xs = (take n xs,drop n xs)
-- Though clear, the above definition is maybe a little inefficient as it involves processing xs twice. Give a definition of splitAt that traverses the list only once.

take' :: Int -> [a] -> [a]
take' _ []     = []
take' n (x:xs) = if n == 0 then [] else x : (take' (n - 1) xs)

drop' :: Int -> [a] -> [a]
drop' _ []     = []
drop' n (x:xs) = if n == 0 then xs else drop' (n - 1) xs

-- take 0 undefined == undefined
-- take undefined [] == []

splitAt' :: Int -> [a] -> ([a], [a])
splitAt' _ []     = ([], [])
splitAt' 0 xs     = ([], xs)
splitAt' n (x:xs) = (x:ls, rs)
    where (ls, rs) = splitAt' (n-1) xs



-- Exercise I
-- Which of the following statements about the equation map (f . g) xs = map f (map g xs)
-- do you agree with, and which do you disagree with (again, no justification is required)?
-- 1. It’s not true for all xs; it depends on whether xs is a finite list or not.
-- 2. It’s not true for all f and g; it depends on whether f and g are strict functions or not.
-- 3. It’s true for all lists xs, finite, partial or infinite, and for all f and g of the appropriate type. In fact map (f . g) = map f . map g is a much neater alternative.
-- 4. It looks true, but it has to be proved so from the definition of map and the definition of functional composition.
-- 5. Used right-to-left, it expresses a program optimisation: two traversals of a list are replaced by one.
-- 6. It’s not an optimisation under lazy evaluation because map g xs is not computed in its entirety before evaluation of map f on the result begins.
-- 7. Whether or not it is computed in pieces or as a whole, the right-hand side does produce an intermediate list, while the left-hand side doesn’t. It is a rule for optimising a program even under lazy evaluation.

-- Answers:
-- 1. False
-- 2. False
-- 3. True
-- 4. True
-- 5. True
-- 6. False
-- 7. True



-- Exercise J
-- Here are some equations; at least one of them is false. Which are the true ones, and which are false? Once again, you do not have to provide any justification for your answers, the aim is just to look at some equations and appreciate what they are saying.
-- map f . take n = take n . map f                             -- True
-- map f . reverse = reverse . map f                           -> True
-- map f . sort = sort . map f                                 -> False
-- map f . filter p = map fst . filter snd . map (fork (f,p))  -> True
-- filter (p . g) = map (invertg) . filter p . map g           -> True
-- reverse . concat = concat . reverse . map reverse           -> True
-- filter p . concat = concat . map (filter p)                 -> True

-- -- In the fifth equation assume invertg satisfies invertg . g = id. The function fork in the fourth equation is defined by
-- fork :: (a -> b,a -> c) -> a -> (b,c)
-- fork (f,g) x = (f x, g x)



-- TODO
-- Exercise K
-- Define unzip and cross by
-- unzip = fork (map fst, map snd)
-- cross (f,g) = fork (f . fst, g . snd)
-- What are the types of these functions?
-- Prove by simple equational reasoning that
-- cross (map f, map g) . unzip = unzip . map (cross (f,g))
-- You can use the functor laws of map and the following rules:
-- cross (f,g) . fork (h,k) = fork (f . h,g . k)
-- fork (f,g) . h = fork (f . h,g . h)
-- fst . cross (f,g) = f . fst
-- snd . cross (f,g) = g . snd



