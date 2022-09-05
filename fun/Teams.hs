module Teams where

teams :: [a] -> ([a], [a])
teams []         = ([], [])
teams [x]        = ([x], [])
teams (x1:x2:xs) = (x1: lxs, x2 : rxs)
    where (lxs, rxs) = teams xs