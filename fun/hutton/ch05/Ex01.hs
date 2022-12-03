module Ex01 where

-- 1. Using a list comprehension, give an expression that calculates the sum 1^2 + 2^2 + ... 100^2 of the first one hundred integer squares.
ex01 :: Integer
ex01 = sum [x ^ 2 | x <- [1..100]]