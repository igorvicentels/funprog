module Nats where

data Nat = Zero 
         | Succ Nat

instance Show Nat where
    show Zero     = "O"
    show (Succ n) = 'S' : show n

plus :: Nat -> Nat -> Nat
plus n Zero     = n
plus n (Succ m) = Succ (plus n m)

