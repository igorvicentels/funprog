module Trees where

data Tree a = Node a (Tree a) (Tree a)
            | Leaf a
            deriving (Show, Eq)

flatten :: Tree a -> [a]
flatten (Leaf x)       = [x]
flatten (Node x t1 t2) = flatten t1 ++ [x] ++ flatten t2

tmap :: (a -> b) -> Tree a -> Tree b
tmap f (Leaf x)       = Leaf (f x)
tmap f (Node x t1 t2) = Node (f x) (tmap f t1) (tmap f t2)