module ExUsing where

import Prelude hiding
    ( filter
    )

type Pred a = (a -> Bool)

-- using concat
filter :: Pred a -> [a] -> [a]
filter p = concat . map (\x -> if p x then [x] else [])

-- using zipWith
sorted :: Ord a => [a] -> Bool
sorted xs = (and . zipWith (<=) xs) (tail xs)

-- using zipWith
fibs :: Integral i => [i]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)


