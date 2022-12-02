module Naturals where

data Nat = Z | S Nat
    deriving ( Show, Eq )

bot :: a
bot = bot

atLeast2 :: Nat
atLeast2 = S $ S $ bot

infinity :: Nat
infinity = S $ infinity

data Natural = Zero | Succ !Natural
    deriving ( Show, Eq )

natLeast2 :: Natural
natLeast2 = Succ $ Succ $ bot

ninfinity :: Natural
ninfinity = Succ $ ninfinity