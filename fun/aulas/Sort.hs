module Sort
    ( sort
    , msort
    , qsort
    , isort
    ) where

sort :: Ord a => [a] -> [a]
sort = msort

-- ASSUMPTION: xs and ys are sorted
merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

-- merge sort
msort :: Ord a => [a] -> [a]
msort []  = []
msort [x] = [x]
msort xs  = merge (msort ys) (msort zs)
    where 
        (ys, zs) = halve xs

halve :: [a] -> ([a], [a])
halve []         = ([], [])
halve [x]        = ([x], [])
halve (x1:x2:xs) = (x1:lxs, x2:rxs)
    where (lxs, rxs) = halve xs

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort small ++ [x] ++ qsort large
    where 
        small = filter (<x) xs
        large = filter (>=x) xs

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

-- ASSUMPTION: For insert w xs, xs is sorted
insert :: Ord a => a -> [a] -> [a]
insert w []         = [w]
insert w xs'@(x:xs)
    | w < x     = w : xs'
    | otherwise = x : insert w xs 


sorted :: Ord a => [a] -> Bool
sorted (x:x':xs) = x <= x' && sorted (x':xs)
sorted _         = True


-- Tests
prop_qsortLength xs = length xs == length (qsort xs)
prop_qsortSorts xs = sorted (qsort xs)
prop_qsortQsort xs = qsort xs == qsort (qsort xs)