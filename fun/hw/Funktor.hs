module Funktor where

import Prelude hiding ( fmap , (<$) )

class Funktor f where
  fmap :: (a -> b) -> f a -> f b

  (<$) :: b        -> f a -> f b
  (<$) = fmap . const

-- Algebraic Laws
-- map id = id
-- map (f . g) = map f . map g

instance Funktor [] where
    fmap = map

instance Funktor Maybe where
    fmap f Nothing  = Nothing
    fmap f (Just x) = Just $ f x

-- what about Either?
instance Funktor (Either a) where
    fmap f (Left x)  = Left x
    fmap f (Right x) = Right $ f x

-- what about pairs?
instance Funktor ((,) a) where
    fmap f (x,y) = (x, f y)

-- what about functions?
instance Funktor ((->) r) where
    fmap = (.)

-- what about Trees?
data Tree a = Leaf a 
            | Node (Tree a) a (Tree a)
    deriving ( Show, Eq )

instance Funktor Tree where
    fmap f (Leaf x)       = Leaf $ f x
    fmap f (Node tl x tr) = Node (fmap f tl) (f x) (fmap f tr)

instance Funktor IO where
    fmap f ax =
        do x <- ax
           return (f x)  

-- ...define Functor instances of as many * -> * things as you can think of!

