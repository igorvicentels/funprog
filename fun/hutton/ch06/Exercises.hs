module Exercises where

-- 1. How does the recursive version of the factorial function behave if applied to a negative argument, such as (-1)? Modify the definition to prohibit negative arguments by adding a guard to the recursive case.

-- Answer:
-- it will make infinite recursive calls
factorial :: Int -> Int
factorial n | n < 0     = error "negative number"
            | n == 0    = 1
            | otherwise = n * factorial (n - 1)
        


-- 2. Define a recursive function sumdown :: Int -> Int that returns the sum of the non-negative integers from a given value down to zero. For example, sumdown 3
-- should return the result 3 + 2 + 1 = 6.

-- Answer:
sundown :: Int -> Int
sundown n | n < 0     = error "negative number"
          | n == 0    = 0
          | otherwise = n + sundown (n - 1)




-- 3. Define the exponentiation operator ^ for non-negative integers using the same pattern of recursion as the multiplication operator *, and show how the expression 2 ^ 3 is evaluated using your definition.

-- Answer:
add :: Int -> Int -> Int
add x 0 = x
add x y = 1 + (add x (y - 1))

mul :: Int -> Int -> Int
mul x 0 = 0
mul x y = x + (mul x (y - 1))

exp' :: Int -> Int -> Int
exp' x 0 = 1
exp' x y = x * (exp' x (y - 1))


-- 4. Define a recursive function euclid :: Int -> Int -> Int that implements Euclid’s algorithm for calculating the greatest common divisor of two non negative integers: if the two numbers are equal, this number is the result; otherwise, the smaller number is subtracted from the larger, and the same process is then repeated. For example:
-- > euclid 6 27
-- 3

-- Answer:
euclid :: Int -> Int -> Int
euclid x y | x == y    = x
           | x < y     = euclid x (y - x)
           | otherwise = euclid (x - y) y



-- 5. Using the recursive definitions given in this chapter, show how length [1,2,3],drop 3 [1,2,3,4,5], and init [1,2,3] are evaluated.
-- length :: [a] -> Int
-- length [] = 0
-- length (_:xs) = 1 + length xs

-- drop :: Int -> [a] -> [a]
-- drop 0 xs = xs
-- drop _ [] = []
-- drop n (_:xs) = drop (n-1) xs

-- init :: [a] -> [a]
-- init [_] = []
-- init (x:xs) = x : init xs

-- Answer:
-- length [1,2,3] = 1 + length [2,3]
--                = 1 + (1 + length [3])
--                = 1 + (1 + (1 + length []))
--                = 1 + (1 + (1 + 0))
--                = 3

-- drop 3 [1,2,3,4,5] = drop 2 [2,3,4,5]
--                    = drop 1 [3,4,5]
--                    = drop 0 [4,5]
--                    = [4,5]

-- init [1,2,3] = 1 : init [2,3]
--              = 1 : 2 : init [3]
--              = 1 : 2 : []
--              = [1,2]



-- 6. Without looking at the definitions from the standard prelude, define the following library functions on lists using recursion.
-- a. Decide if all logical values in a list are True:
-- and :: [Bool] -> Bool
-- b. Concatenate a list of lists:
-- concat :: [[a]] -> [a]
-- c. Produce a list with n identical elements:
-- replicate :: Int -> a -> [a]
-- d. Select the nth element of a list:
-- (!!) :: [a] -> Int -> a
-- e. Decide if a value is an element of a list:
-- elem :: Eq a => a -> [a] -> Bool
-- Note: most of these functions are defined in the prelude using other library functions rather than using explicit recursion, and are generic functions rather than being specific to the type of lists.

-- Answer:
and' :: [Bool] -> Bool
and' []     = True
and' (x:xs) = x && and' xs

concat' :: [[a]] -> [a]
concat' []     = []
concat' (x:xs) = x ++ (concat xs)

replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = x : replicate (n - 1) x

(<!!>) :: [a] -> Int -> a
(<!!>) []     _ = error "index out of bounds"
(<!!>) (x:xs) 0 = x 
(<!!>) (x:xs) n = (<!!>) xs (n - 1)

elem' :: Eq a => a -> [a] -> Bool
elem' _ []     = False
elem' w (x:xs) = x == w || elem' w xs



-- 7. Define a recursive function merge :: Ord a => [a] -> [a] -> [a] that merges two sorted lists to give a single sorted list. For example:
-- > merge [2,5,6] [1,3,4]
-- [1,2,3,4,5,6]
-- Note: your definition should not use other functions on sorted lists such as insert or sort, but should be defined using explicit recursion.

-- Answer:
merge :: Ord a => [a] -> [a] -> [a]
merge []         ys         = ys
merge xs         []         = xs
merge xs'@(x:xs) ys'@(y:ys) | x <= y    = x : merge xs ys'
                            | otherwise = y : merge xs' ys



-- 8. Using merge, define a function msort :: Ord a => [a] -> [a] that implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.
-- Hint: first define a function halve :: [a] -> ([a],[a]) that splits a list into two halves whose lengths differ by at most one.

-- Answer:
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort left) (msort right)
    where 
        (left, right) = halve xs
        halve ls = splitAt ((length ls) `div` 2) ls



-- 9. Using the five-step process, construct the library functions that:
-- a. calculate the sum of a list of numbers;
-- b. take a given number of elements from the start of a list;
-- c. select the last element of a non-empty list.

--Answer:
sum' :: [Int] -> Int
sum' []     = 0
sum' (x:xs) = x + (sum' xs)

take' :: Int -> [a] -> [a]
take' n _      
    | n <= 0   = []
take' _ []     = []
take' n (x:xs) = x : (take' (n-1) xs)

last' :: [a] -> a
last' []     = error "empty list"
last' [x]    = x
last' (x:xs) = last' xs
