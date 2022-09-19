module Map where

import Prelude hiding (map)

map :: (a -> b) -> [a] -> [b]
map f = \ xs -> 
    case xs of
        [] -> []
        (x:xs) -> f x : map f xs