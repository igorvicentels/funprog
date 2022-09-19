module Nats where

data Nat = Zero 
         | Succ Nat
    deriving (Eq)

instance Show Nat where
    show Zero     = "O"
    show (Succ n) = 'S' : show n

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = Succ (plus n m)

len :: [a] -> Nat
len []     = Zero
len (x:xs) = Succ (len xs)

rev :: [a] -> [a]
rev []     = []
rev (x:xs) = rev xs ++ [x]

atLeastTwo :: [a] -> Bool
atLeastTwo []     = False
atLeastTwo [x] = False
atLeastTwo _   = True
-- atLeastTwo (x:xs) = 
--     case xs of
--         []    -> False
--         (_:_) -> True

firstTwo :: [a] -> (a,a)
firstTwo []      = error "empty list"
firstTwo [x]     = error "singleton list"
firstTwo (x:y:_) = (x, y)


describe :: [a] -> String
describe xs
    | l == 0    = "empty"
    | l < 3     = "small"
    | l < 6     = "big"
    | otherwise = "too big"
    where l = length xs