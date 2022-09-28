module Examples where

map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' p xs = [x | x <- xs, p x]

sumsqreven :: [Int] -> Int
sumsqreven =  sum . map (^2) . filter even

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f v []     = v
foldr' f v (x:xs) = f x (foldr f v xs) 

length' :: [a] -> Int
length' = foldr' (\_ n -> 1 + n) 0

reverse' :: [a] -> [a]
reverse' = foldr' snoc []
    where snoc x xs = xs ++ [x]

foldl' :: (a -> b -> a) -> a -> [a] -> a
foldl'f v []     = v
foldl'f v (x:xs) = foldl' f (f v x) xs