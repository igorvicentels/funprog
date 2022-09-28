-- 1. Show how the list comprehension [f x | x <- xs, p x] can be re-expressed using the higher-order functions map and filter. 
mapfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapfilter f p xs = map f (filter p xs)

-- 2. Without looking at the definitions from the standard prelude, define the following higher-order library functions on lists.
    -- a. Decide if all elements of a list satisfy a predicate:
        -- all :: (a -> Bool) -> [Bool] -> Bool
    -- b. Decide if any element of a list satisfies a predicate:
        -- any :: (a -> Bool) -> [Bool] -> Bool
    -- c. Select elements from a list while they satisfy a predicate:
        -- takeWhile :: (a -> Bool) -> [a] -> [a]
    -- d. Remove elements from a list while they satisfy a predicate:
        -- dropWhile :: (a -> Bool) -> [a] -> [a]