module Desafios where

import Prelude hiding (filter)

filter :: (a -> Bool) -> [a] -> [a]
filter p = concat . map (\x -> if p x then [x] else [])

sorted :: Ord a => [a] -> Bool
sorted []     = True
sorted (x:xs) = and $ zipWith (<=) (x:xs) xs
