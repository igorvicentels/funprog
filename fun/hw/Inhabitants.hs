module Inhabitants where

-- Define *convergent* (not bottom-ish) inhabitants
-- of the following types.
-- For each type, if you can define more than one
-- inhabitant, define a few of them, using primes
-- to name the other inhabitants, for example:
-- f, f', f'', f''', f'''', etc.

cI :: a -> a
cI x = x

cK :: a -> b -> a
cK x y = x

cS :: (a -> b -> c) -> (a -> b) -> (a -> c)
cS f g x = f x (g x)

cB :: (b -> c) -> ((a -> b) -> (a -> c))
cB f g x = f (g x)
-- cB = (.)

cW :: (a -> (a -> b)) -> (a -> b)
cW f x = f x x

pL :: ((a -> b) -> a) -> a
pL = undefined

p1 :: (a,b) -> a
p1 (x,_) = x

p2 :: (a,b) -> b
p2 (_,y) = y

p3 :: (a,b) -> (b,a)
p3 (x,y) = (y,x)

p4 :: Either a b -> b
p4 (Left x)  = error "a"
p4 (Right y) = y

p5 :: Either a b -> [a]
p5 (Left x)  = [x]
p5 (Right y) = error "a"

p6 :: Either a b -> (a,b)
p6 = undefined

p7 :: (a,b) -> Either a b
p7 (x,y) = Left x 

p8 :: (a,b) -> Either a b
p8 (x,y) = Right y 

p9 :: a -> b -> (a,a,b)
p9 x y = (x,x,y)

q1 :: a -> a -> a
-- q1 x y = x
q1 x y = y

q2 :: (a,a) -> (a,a)
-- q2 (x,y) = (x,y) 
q2 (x,y) = (y,x) 

q3 :: Either (a,b) (a,c) -> (a, Either b c)
-- q3 (Left (x,y))  = (x, Left y)
q3 (Right (x,z)) = (x, Right z)

l1 :: [a]
l1 = []

l2 :: [[a]]
-- l2 = []
l2 = [[]]

l3 :: [a] -> [a]
-- l3 [] = []
-- l3 (x:xs) = xs
l3 xs = xs

l4 :: a -> [a]
-- l4 x = []
-- l4 x = [x]
l4 x = [x,x]

l5 :: [a] -> a
l5 []     = error "empty list"
l5 (x:xs) = x

l6 :: a -> [[a]]
-- l6 x = [[x]]
l6 x = [[x,x]]

