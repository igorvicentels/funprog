module ExEither where

-- Do not alter this import!
import Prelude hiding ( either, Either(..) )
import qualified Data.Either as E

data Either a b = Left a | Right b
    deriving (Show, Eq)

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _         = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

lefts :: [Either a b] -> [a]
lefts [] = [] 
lefts (Left x : xs) = x : lefts xs 
lefts (_      : xs) = lefts xs 

rights :: [Either a b] -> [b]
rights [] = []
rights (Right x : xs) = x : rights xs
rights (_       : xs) = rights xs

fromLeft :: a -> Either a b -> a
fromLeft _ (Left x)  = x
fromLeft d (Right _) = d

fromRight :: b -> Either a b -> b
fromRight d (Left _) = d
fromRight _ (Right x) = x

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers []            = ([], [])
partitionEithers (ex : xs) = 
    case ex of
        Left l  -> (l : ls, rs)
        Right r -> (ls, r : rs)
    where (ls,rs) = partitionEithers xs

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f g (Left x)  = f x
either f g (Right x) = g x

