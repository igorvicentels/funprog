module Arith where

-- define a  type Arith to represent arithmetic expressions:
-- addition, multiplication, and nothing else

data Arith = Atom Integer
           | Plus Arith Arith    -- Arith :+: Arith
           | Times Arith Arith   -- Arith :*: Arith

-- better show
instance Show Arith where
    show (Atom n)    = show n
    show (Plus s t)  = showOp " + " s t
    show (Times s t) = showOp " * " s t

showOp op s t = parenthesize "(" ")" $ show s ++ op ++ show t

parenthesize o c s = o ++ s ++ c

-- val evaluates an expression and returns its value
val :: Arith -> Integer
val (Atom n)    = n
val (Plus s t)  = val s + val t
val (Times s t) = val s * val t

val' :: [(String, Integer)] -> Arith -> Integer
val' [] x = val' x
val' 

step :: Arith -> Arith
step (Atom n) = (Atom n)
step (Plus s t)

-- (Plus (Atom 5) (Times (Plus (Atom 4) (Atom 3)) (Times (Atom 8) (Atom 7))))