module ExRat
    ( rat
    , (//)
    , denominator
    , numerator
    ) where

-- define Rat:
data Rat = Rat Integer Integer

instance Show Rat where
    show (Rat p q) = "(" ++ show p ++ "/" ++ show q ++ ")"

instance Eq Rat where
    Rat p q == Rat p' q' = p * q' == p' * q

instance Num Rat where
    (+) (Rat x y) (Rat x' y') = rat (x + x') (y + y') 

    (*) (Rat x y) (Rat x' y') = rat (x * x') (y * y')

    negate (Rat x y) = rat (-x) y

    abs (Rat x y) = if (x * y >= 0) 
                    then rat x y 
                    else rat (-x) y

    signum = undefined
    fromInteger = undefined

instance Ord Rat where
    compare = undefined

rat :: Integer -> Integer -> Rat
rat p 0 = error "division by zero"
rat p q = Rat p q

(//) :: Rat -> Rat -> Rat
Rat p q // Rat p' q' = Rat (p * q') (q * p')

denominator :: Rat -> Integer
denominator (Rat p q) = q

numerator :: Rat -> Integer
numerator (Rat p q) = p

