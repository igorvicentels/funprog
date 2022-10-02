module Nats where

data Nat = Zero | Succ Nat
    deriving ( Eq )

instance Show Nat where
    show Zero     = "O"
    show (Succ x) = 'S' : show x

bottom :: Nat
bottom = bottom

atLeastTwo :: Nat
atLeastTwo = Succ (Succ bottom)

atLeast :: Nat -> Nat
atLeast Zero     = bottom
atLeast (Succ x) = Succ (atLeast x)

infinite :: Nat
infinite = Succ infinite
