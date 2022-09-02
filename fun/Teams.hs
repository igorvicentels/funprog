module Teams where

teams :: [a] -> ([a], [a])
teams []         = ([], [])
teams [x]        = ([x], [])
teams (x1:x2:xs) = (x1: fst (teams xs), x2 : snd (teams xs))
