module EitherVsPair where

data Pair a b = Pair a b

data Either a b = Left a 
                | Right b

id :: a -> a
id x = x

const' :: a -> b -> a
-- const x _ = x
const' x = \_ -> x

h :: c -> (a, b)
h x = (f x, g x) 
    where
        f :: (c -> a)
        f = undefined
        g :: (c -> b)
        g = undefined