module CommonWords where

import Data.List (sort)
import Data.Char (toLower)

commonWords :: Int -> [Char] -> [Char]
commonWords n = concat . map showRun . take n .
                sortRuns . countRuns . sortWords .
                words . map toLower

showRun :: (Int, [Char]) -> [Char]
showRun (n,w) = w ++ ": " ++ show n ++ "\n"

countRuns :: [[Char]] -> [(Int,[Char])]
countRuns []     = []
countRuns (w:ws) = (1+length us,w):countRuns vs
                    where (us,vs) = span (==w) ws

sortWords :: [[Char]] -> [[Char]]
sortWords = sort

sortRuns :: [(Int,[Char])] -> [(Int,[Char])]
sortRuns = reverse . sort