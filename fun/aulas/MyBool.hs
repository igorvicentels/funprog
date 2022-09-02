module MyBool where

data Boolean = F | T

lnot :: Boolean -> Boolean
lnot F = T
lnot T = F

ifthenelse :: Boolean -> a -> a -> a
ifthenelse T x _ = x
ifthenelse F _ y = y