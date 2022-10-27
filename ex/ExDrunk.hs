module ExDrunk
    ( atIndices
    , everyOther
    , disjoint
    , stretch
    , drunk
    ) where

-- example:
-- atIndices [1,4,5] "Tchauzinho"
-- = "cuz"
atIndices :: Integral i => [i] -> [a] -> [a]
atIndices (0:xs) (y:ys) = y : atIndices (map (\x -> x - 1) xs) ys 
atIndices (x:xs) (y:ys) = atIndices (map (\x -> x - 1) (x:xs)) ys
atIndices _      _      = [] 

-- example:
-- everyOther 2 "Hello There"
-- = "HloTee"
everyOther :: Integral i => i -> [a] -> [a]
everyOther _ []     = [] 
everyOther n (x:xs) 
    | length xs < m = [x]
    | otherwise     = x : everyOther n (drop (m - 1) xs)
     where m = fromIntegral n
-- examples:
-- disjoint [1,5,9] [2 .. 6]
-- = False
-- disjoint [1,5,9] [2,4 ..]
-- = True
-- ASSUMPTIONS FOR disjoint xs ys:
--   xs and ys are sorted
disjoint :: Ord a => [a] -> [a] -> Bool
disjoint (x:xs) ys = all (x/=) ys && disjoint xs ys
disjoint _      _  = True
-- example:
-- stretch 3 "Gustavo"
-- = "GGGuuussstttaaavvvooo"
stretch :: Integral i => i -> [a] -> [a]
stretch n = concat . map (replicate (fromIntegral n))

-- example:
-- drunk 3 "Gustavo"
-- = "GusGtuasvtoavo"
-- drunk 5 "Gustavo"
-- = "GustaGvuostavo"
-- To understand these string, either get drunk or look at the markings:
--       , , , , ,,,
--   "GusGtuasvtoavo"
--    ''' ' ' ' '
--         , , ,,,,,
--   "GustaGvuostavo"
--    ''''' ' '
drunk :: Integral i => i -> [a] -> [a]
drunk n []         = []
drunk n xs'@(x:xs) = take m xs' ++ f m xs'
    where
        m = fromIntegral n
        f n []         = [] 
        f n ys'@(y:ys) 
            | length ys' > n = y : head (drop n ys') : f n ys
            | otherwise = y:ys