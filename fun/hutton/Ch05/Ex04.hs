module Ex04 where

-- 4. In a similar way to the function length, show how the library function replicate :: Int -> a -> [a] that produces a list of identical elements can be defined using a list comprehension. For example:
-- > replicate 3 True
-- [True,True,True]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

replicate' :: Int -> a -> [a]
replicate' n x = [x | _ <- [1..n]]

