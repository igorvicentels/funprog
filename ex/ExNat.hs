module ExNat where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral
    , Bool(..)
    , not
    , (&&)
    , (||)
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat = Zero | Succ Nat

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show Zero     = "O"
    show (Succ n) = 'S' : show n

instance Eq Nat where

    Zero     == Zero     = True
    Zero     == (Succ _) = False
    (Succ _) == Zero     = False
    (Succ n) == (Succ m) = n == m

instance Ord Nat where

    Zero     <= _        = True
    (Succ n) <= Zero     = False
    (Succ n) <= (Succ m) = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min (Succ n) (Succ m) = Succ (min n m)
    min _        _        = Zero

    max Zero     m        = m
    max n        Zero     = n
    max (Succ n) (Succ m) = Succ (max n m)

isZero :: Nat -> Bool
isZero Zero = True
isZero _    = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred Zero     = Zero
pred (Succ n) = n

even :: Nat -> Bool
even Zero            = True
even (Succ Zero)     = False
even (Succ (Succ n)) = even n

odd :: Nat -> Bool
odd n = not (even n) 

-- addition
(<+>) :: Nat -> Nat -> Nat
Zero   <+> m = m
Succ n <+> m = Succ (n <+> m)

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.
(<->) :: Nat -> Nat -> Nat
Zero     <-> _        = Zero
n        <-> Zero     = n
(Succ n) <-> (Succ m) = n <-> m 
 
-- multiplication
(<*>) :: Nat -> Nat -> Nat
Zero     <*> _ = Zero
(Succ n) <*> m = (n <*> m) <+> m

-- exponentiation
(<^>) :: Nat -> Nat -> Nat
n <^> Zero     = Succ Zero
n <^> (Succ m) = n <*> (n <^> m)

-- quotient
(</>) :: Nat -> Nat -> Nat
x </> y 
    | x < y     = Zero
    | otherwise = Succ ((x <-> y) </> y)

-- remainder
(<%>) :: Nat -> Nat -> Nat
x <%> y 
    | x < y = x
    | otherwise = (x <-> y) <%> y

-- divides
(<|>) :: Nat -> Nat -> Bool
x <|> y = y <%> x == Zero

divides = (<|>)


-- x `absDiff` y = |x - y|
-- (Careful here: this - is the real minus operator!)
absDiff :: Nat -> Nat -> Nat
absDiff x y = max x y - min x y

(|-|) = absDiff

factorial :: Nat -> Nat
factorial Zero     = Succ Zero
factorial (Succ n) = Succ n <*> factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg Zero = Zero
sg _    = Succ Zero

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo b a 
    | b > a     = Zero 
    | otherwise = Succ (lo b (a </> b))


--
-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!
--

toNat :: Integral a => a -> Nat
toNat x
    | x < 0     = error "cannot convert negative number to Nat"
    | x == 0    = Zero
    | otherwise = Succ (toNat (x -1))

fromNat :: Integral a => Nat -> a
fromNat Zero     = 0
fromNat (Succ n) = 1 + fromNat n


-- Obs: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger = toNat

