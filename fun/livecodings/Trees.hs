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

nLeafs :: Tree a -> Int
nLeafs (Leaf x)       = 1
nLeafs (Node x tl tr) = nLeafs tl + nLeafs tr

nNodes :: Tree a -> Int
nNodes (Leaf x)       = 0
nNodes (Node x tl tr) = 1 + nNodes tl + nNodes tr

t1 = Node 1 (Node 2 (Leaf 3) (Node 4 (Leaf 5) (Leaf 6))) (Leaf 3) 