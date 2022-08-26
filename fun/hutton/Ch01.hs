module Ch01 where

import Prelude hiding ( sum, product )

---------------------------------------------------------------

-- Examples

---------------------------------------------------------------    

sum :: Num a => [a] -> a
sum []     = 0
sum (x:xs) = x + sum xs

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort bigger
    where smaller = [a | a <- xs, a <= x]
          bigger = [a | a <- xs , a > x]

seqn :: Monad m => [m a] -> m [a]
seqn []         = return []
seqn (act:acts) = do
    x <- act
    xs <- seqn acts
    return (x:xs)


---------------------------------------------------------------

-- Exercises

---------------------------------------------------------------

-- 1. Give another possible calculation for the result of double (double 2).
    -- double (double 2) = double (2 + 2)         
    --                     (2 + 2) + (2 + 2)
    --                     4 + (2 + 2)
    --                     4 + 4
    --                     8


-- 2. Show that sum [x] “ x for any number x.
    -- sum [x] = x + sum []    [sum2] 
    --           x + 0         [sum1]
    --           x             [+]

-- 3. Define a function product that produces the product of a list of numbers, 
-- and show using your definition that product [2,3,4] = 24.
product :: Num a => [a] -> a
product []     = 1
product (x:xs) = x * product xs

    -- product [2,3,4] = 2 * product [3,4]
    --                   2 * (3 * product [4])
    --                   2 * (3 * (4 * product []))
    --                   2 * (3 * (4 * 1))
    --                   24


-- 4. How should the definition of the function qsort be modified so that it 
-- produces a reverse sorted version of a list?
qsort' :: Ord a => [a] -> [a]
qsort' []     = []
qsort' (x:xs) = qsort' bigger ++ [x] ++ qsort' smaller
    where bigger = [a | a <- xs, a > x]
          smaller = [a | a <- xs, a < x]


-- 5. What would be the effect of replacing <= by < in the original definition of
-- qsort? Hint: consider the example qsort [2,2,3,1,1].
    -- the function would delete the repeated values