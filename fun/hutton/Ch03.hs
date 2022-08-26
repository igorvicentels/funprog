module Ch03 where

---------------------------------------------------------------

-- Exercises

---------------------------------------------------------------

-- 1. What are the types of the following values?
    -- [’a’,’b’,’c’]              :: [Char]
    -- (’a’,’b’,’c’)              :: (Char, Char, Char)
    -- [(False,’O’),(True,’1’)]   :: [(Bool, Char)]
    -- ([False,True],[’0’,’1’])   :: ([Bool], [Char])
    -- [tail, init, reverse]      :: [[a] -> [a]]

-- 2. Write down definitions that have the following types; it does not matter what
-- the definitions actually do as long as they are type correct.
    -- bools :: [Bool]                   => bools = [True, True]
    -- nums :: [[Int]]                   => nums = [[1 :: Int]]
    -- add :: Int -> Int -> Int -> Int   => add = \x y z -> x + y + z :: Int 
    -- copy :: a -> (a,a)                => copy = \x -> (x,x)
    -- apply :: (a -> b) -> a -> b       => apply = \f x -> f x

-- 3. What are the types of the following functions?
    -- second xs = head (tail xs)         :: [a] -> a
    -- swap (x,y) = (y,x)                 :: (a, b) -> (b, a)
    -- pair x y = (x,y)                   :: a -> b -> (a, b)
    -- double x = x*2                     :: Num a => a -> a
    -- palindrome xs = reverse xs == xs   :: Eq a => [a] -> Bool
    -- twice f x = f (f x)                :: (a -> a) -> a -> a

-- 4. Check your answers to the preceding three questions using GHCi.

-- TODO
-- 5. Why is it not feasible in general for function types to be instances of the Eq
-- class? When is it feasible? Hint: two functions of the same type are equal if
-- they always return equal results for equal arguments.