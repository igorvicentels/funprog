module TillRep where

tillRep :: Eq a => [a] -> [a]
tillRep []         = []
tillRep [x]        = [x] 
tillRep (x1:x2:xs) = if x1 == x2 
                        then [x1] 
                        else x1 : tillRep (x2:xs) 