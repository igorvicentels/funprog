module Mutrec where

data Nat = Zero | Succ Nat
    deriving ( Show, Eq )

even :: Nat -> Bool
even Zero     = True
even (Succ n) = odd n

odd :: Nat -> Bool
odd Zero     = False
odd (Succ n) = even n