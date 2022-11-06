module Mentira where

data Tipo = TipoInt Int
          | TipoBool Bool
          | TipoChar Char
          deriving ( Show, Eq )

f :: Tipo -> Tipo
f = undefined

x :: Tipo
x = undefined

truezinho :: Tipo
truezinho = TipoBool True

onezinho :: Tipo
onezinho = TipoInt 1

succinho :: Tipo -> Tipo
succinho x = 
    case x of
        TipoInt y -> TipoInt (y + 1)
        _         -> error "runtime error"
