module Box where

data Box a = Box a
    deriving ( Show, Eq )

data Booleano = Verdade | Falso
    deriving ( Show, Eq )

-- data Bool = True | False
--     deriving ( Show, Eq )

data Two = Zero | One
    deriving ( Show, Eq )

data Bit = O | I
    deriving ( Show, Eq )

matamosca :: (Box Bool) -> Int
matamosca (Box True) = 1 
matamosca (Box False) = 0 

-- Diferenciando BottomBool de BottomBoxBool
matamosca' :: (Box Bool) -> String
matamosca' (Box x) = "Algo boxado"
matamosca' x = "Não boxadado"

bottom :: a
bottom = bottom

outrobottom :: Box Bool
outrobottom = error "oi"
outrobottom' = undefined
