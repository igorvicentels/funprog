module Freefun where

cross :: ((a -> b), (c -> d)) -> (a, c) -> (b, d)
cross (f, g) (x, y) = (f x, g y) 

pair :: ((a -> b), (a -> c)) -> a -> (b, c)
pair (f, g) x = (f x, g x)

dup :: a -> (a,a)
dup = pair (id, id)

-- h = 2x + 1
h :: Int -> Int
h = succ . uncurry (+) . dup 

-- f = 2x^2
f :: Int -> Int 
f = uncurry (+) . dup . uncurry (*) . dup

-- g = x^2 + 2y + xy^2
-- g :: (Int, Int) -> Int
-- g = uncurry (+) . cross ((uncurry (*) . dup), (uncurry (+). dup))