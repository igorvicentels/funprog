module List where

data ListInt = Nil | Cons Int ListInt
    deriving (Eq)

instance Show ListInt where
    show x = "[" ++ show' x ++ "]"
        where show' Nil          = ""
              show' (Cons x Nil) = show x
              show' (Cons x xs)  = show x ++ ", " ++ show' xs

hd :: ListInt -> Int
hd Nil = error "empty list"
hd (Cons x _) = x

tl :: ListInt -> ListInt
tl Nil         = error "empty list"
tl (Cons _ xs) = xs