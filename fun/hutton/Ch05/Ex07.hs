module Ex07 where

-- 7. Show how the list comprehension [(x,y) | x <- [1,2], y <- [3,4]] with two generators can be re-expressed using two comprehensions with single generators. Hint: nest one comprehension within the other and make use of the library function concat :: [[a]] -> [a].

ex07 :: [Int] -> [Int] -> [(Int, Int)]
ex07 xs ys = concat [[(x,y) | y <- ys] | x <- xs]

ex07_3d xs ys zs = concat [ concat [[(x,y,z) | z <- zs] | y <- ys] | x <- xs]
