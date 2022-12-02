module Sum where

concatr :: [[a]] -> [a]
concatr = foldr (++) []

concatl :: [[a]] -> [a]
concatl = foldl (++) []

