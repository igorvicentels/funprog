module Funky where

data Besta a = Coisa
             | Coiso
             | Algo a
             deriving (Show, Eq)

instance Functor Besta where
    fmap f Coisa    = Coisa
    fmap f Coiso    = Coiso
    fmap f (Algo x) = Algo (f x)


nested :: [Maybe String]
nested = [Just "oi", Nothing, Just "hello"]

trocape :: a -> Char
trocape _ = 'p'

-- (fmap . fmap) trocape nested = [Just 'p',Nothing,Just 'p']
-- (fmap . fmap . fmap) trocape nested = [Just "pp",Nothing,Just "ppppp"]