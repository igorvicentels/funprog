module Ch02 where

---------------------------------------------------------------

-- Exercises

---------------------------------------------------------------

-- 1. Work through the examples from this chapter using GHCi.


-- 2. Parenthesise the following numeric expressions:
    -- 2^3*4 = (2 ^ 3) * 4
    -- 2*3+4*5 = (2 * 3) + (4 * 5)
    -- 2+3*4^5 = 2 + (3 * (4 ^ 5))

-- 3. The script below contains three syntactic errors. Correct these errors and then
-- check that your script works properly using GHCi.
-- N = a `div`length xs
    -- where
        -- a = 10
        -- xs = [1,2,3,4,5]

n = a `div` length xs
    where
        a = 10
        xs = [1,2,3,4,5]

-- 4. The library function last selects the last element of a non-empty list; for ex-
-- ample, last [1,2,3,4,5] “ 5. Show how the function last could be defined
-- in terms of the other library functions introduced in this chapter. Can you
-- think of another possible definition?
last' :: [a] -> a
last' xs = head (reverse xs)

last'' :: [a] -> a
last'' xs = xs !! (length xs - 1)

-- 5. The library function init removes the last element from a non-empty list; for
-- example, init [1,2,3,4,5] = [1,2,3,4]. Show how init could similarly
-- be defined in two different ways.
init' :: [a] -> [a]
init' []     = error "empty list"
init' [x]    = []
init' (x:xs) = x : init' xs

init'' :: [a] -> [a]
init'' []     = error "empty list"
init'' [x]    = []
init'' (x:xs) = x : init'' xs
