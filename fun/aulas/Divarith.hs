{-# LANGUAGE DeriveFunctor             #-}
module Divarith where

data Expr = Val Int
          | Div Expr Expr
    deriving ( Show, Eq )

data Talvez a = Nada
              | Apenas a
    deriving ( Show, Eq, Functor )

instance Applicative Talvez where
    pure = Apenas

    Apenas f <*> Apenas x = Apenas $ f x
    _        <*> _        = Nada

instance Monad Talvez where
    Nada     >>= _  = Nada 
    Apenas x >>= fm = fm x

expr1, expr2 :: Expr
expr1 = (Val 50) `Div` ((Val 10) `Div` (Val 2))
expr2 = (Val 50) `Div` ((Val 10) `Div` (Val 12))

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x `div` y

eval :: Expr -> Maybe Int
eval (Val x)     = Just x
eval (Div e1 e2) = 
    -- bind
    eval e1 >>= \x -> eval e2 >>= \y -> safeDiv x y
    
    -- do
    -- do x <- eval e1
    --    y <- eval e2
    --    safeDiv x y

    -- without monad
    -- case eval e1 of
    --     Nothing -> Nothing
    --     Just x -> 
    --         case eval e2 of
    --             Nothing -> Nothing
    --             Just y -> safeDiv x y