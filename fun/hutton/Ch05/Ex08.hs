module Ex08 where

-- 8. Redefine the function using the function positions find.

find :: Eq a => a -> [(a,b)] -> [b]
find k t = [v | (k',v) <- t, k == k']

-- positions ::  Eq a => a -> [a] -> [Int]
positions x xs = find x [(k,v) | (k,v) <- zip xs [0..]]