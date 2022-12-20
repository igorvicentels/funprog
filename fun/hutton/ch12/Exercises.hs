module Exercises where

-- 1.
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving ( Show ) 

instance Functor Tree where
    fmap f Leaf           = Leaf
    fmap f (Node tl x tr) = Node (fmap f tl) (f x) (fmap f tr) 

tree :: Tree Int
tree = Node Leaf 2 (Node (Node Leaf 3 Leaf) 1 Leaf)



-- 2.
-- instance Functor ((->) a) where
    -- fmap :: (b -> c) -> (a -> b) -> (a -> c)
    -- fmap = (.)



-- 3.
-- instance Applicative ((->) a) where
    -- pure :: b -> (a -> b)
    -- pure = const

    -- <*> :: (a -> b -> c) -> (a -> b) -> (a -> c)
    -- af <*> g = \x -> af x (g x)



-- 4.
newtype ZipList a = Z [a] deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (Z xs) = Z (map g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = Z $ repeat x

    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    (Z gs) <*> (Z xs) = Z $ zipWith ($) gs xs



-- 5.
-- pure :: a -> f a
-- <*> :: f (a -> b) -> f a -> f b

-- pure id <*> x = x
-- id            : (a -> a)
-- x             : f a
-- pure id       : f (a -> a)
-- pure id <*> x : f  a

-- pure (g x) = pure g <*> pure x
-- x                 : a
-- g                 : a -> b
-- g x               : b
-- pure (g x)        : f b
-- pure g            : f (a -> b)
-- pure x            : f a
-- pure g <*> pure x : f b

-- x <*> pure y = pure (\g -> g y) <*> x
-- y                      : a
-- x                      : f (a -> b)
-- pure y                 : f a
-- x <*> pure y           : f b
-- g                      : a -> b
-- \g -> g y              : (a -> b) -> b
-- pure (\g -> g y)       : f ((a -> b) -> b)
-- pure (\g -> g y) <*> x : f b

-- x <*> (y <*> z) = (pure (.) <*> x <*> y) <*> z
-- x                            : f (b -> c)
-- y                            : f (a -> b)
-- z                            : f a
-- y <*> z                      : f b
-- x <*> (y <*> z)              : f c
-- pure (.)                     : f ((b -> c) -> (a -> b) -> (a -> c))
-- pure (.) <*> x               : f ((a -> b) -> (a -> c))
-- pure (.) <*> x <*> y         : f (a -> c)
-- (pure (.) <*> x <*> y) <*> z : f c



-- 6.
-- instance Monad ((->) a) where
-- >>= :: (a -> b) -> (b -> a -> c) -> (a -> c)
-- mx >>= fmb = \y -> fmb (mx y) y



-- 7.
data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
    deriving Show

expr1 = Add (Add (Var 3) (Val 7)) (Val 3)

instance Functor Expr where
    fmap f (Var x)     = Var (f x)
    fmap f (Add e1 e2) = Add (fmap f e1) (fmap f e2)
    fmap f (Val x)     = Val x

instance Applicative Expr where
    pure = Var

    _           <*> (Val x)     = Val x
    (Val x)     <*> _           = Val x
    (Var f)     <*> (Var x)     = Var (f x)
    (Var f)     <*> (Add e1 e2) = Add (fmap f e1) (fmap f e2)
    (Add f1 f2) <*> x           = Add (f1 <*> x) (f2 <*> x)

instance Monad Expr where
    (Val x)     >>= _   = (Val x)
    (Var x)     >>= fmy = fmy x
    (Add e1 e2) >>= fmy = Add (e1 >>= fmy) (e2 >>= fmy)



-- 8. 
type State = Int
newtype ST a = S (State -> (a,State))

app :: ST a -> State -> (a,State)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a -> ST b
    fmap g st = do x <- st
                   S (\y -> (g x, y))

instance Applicative ST where
    -- pure :: a -> ST a
    pure x = S (\s -> (x,s))
    -- (<*>) :: ST (a -> b) -> ST a -> ST b
    stf <*> stx = do f <- stf
                     x <- stx
                     S (\y -> (f x, y))
                      

instance Monad ST where
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S (\s ->
        let (x,s') = app st s in app (f x) s')