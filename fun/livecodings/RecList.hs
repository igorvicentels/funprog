module RecList where

(+++) :: [a] -> [a] -> [a]
[]     +++ ys = ys
(x:xs) +++ ys = x : (xs +++ ys)