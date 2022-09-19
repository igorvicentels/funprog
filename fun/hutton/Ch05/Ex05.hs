module Ex05 where

-- 5. A triple (x, y, z) of positive integers is Pythagorean if it satisfies the equation x^2 + y^2 = z^2. Using a list comprehension with three generators, define a function pyths :: Int -> [(Int,Int,Int)] that returns the list of all such triples whose components are at most a given limit. For example:
-- > pyths 10
-- [(3,4,5),(4,3,5),(6,8,10),(8,6,10)]

pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]