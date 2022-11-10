module ExSet
    ( Set
    , empty
    , singleton
    , fromList
    , toList
    , powerSet
    , insert
    , delete
    , member
    , notMember
    , null
    , size
    , isSubsetOf
    , isProperSubsetOf
    , disjoint
    , pairwiseDisjoint
    , union
    , inter
    , (\\)
    , unions
    , inters
    , cartesianProduct
    , disjointUnion
    , filter
    , partition
    , map
    ) where

import qualified Data.List as L

import Prelude hiding 
    ( filter
    , map
    )

data Set a = Set [a]

-- CAUTION: you may need to add constraints to your types and instances!

instance Eq (Set a) where
    xs == ys  = undefined

instance Show a => Show (Set a) where
    show (Set xs) = "{" ++ show' xs ++ "}"
        where show' []     = ""
              show' [x]    = show x
              show' (x:xs) = show x ++ ", " ++ show' xs

-- smart constructor
set :: Eq a => [a] -> Set a
set = fromList 

empty :: Set a -> Bool
empty (Set []) = True
empty _        = False

singleton :: a -> Set a
singleton x = Set [x]

fromList :: Eq a => [a] -> Set a
fromList xs = Set (aux xs) 
    where aux []     = []
          aux (x:xs) = x : (aux (L.filter (/= x) xs))

toList :: Set a -> [a]
toList (Set xs) = xs

powerSet :: Set a -> Set (Set a)
powerSet = undefined
-- powerSet (Set [])     = (Set (Set [])) 
-- powerSet (Set (x:xs)) = powerSet xs 

-- pset [1,2]   =               {},           {1},   {2},   {1,2}
-- pset [1,2,3] = {}, {1}, {2}, {3}, {1,2}, {1,3}, {2,3}, {1,2,3}    

insert :: Ord a => a -> Set a -> Set a
insert w (Set xs) = set (L.insert w xs) 

delete :: Eq a => a -> Set a -> Set a
delete w (Set xs) = set (L.delete w xs)

member :: Eq a => a -> Set a -> Bool
member w (Set xs) = L.elem w xs

notMember :: Ord a => a -> Set a -> Bool
notMember w s = not (member w s)

null' :: Set a -> Bool
null' (Set xs) = null xs

size :: Integral i => Set a -> i
size (Set xs) = fromIntegral (L.length xs)

isSubsetOf :: Eq a => Set a -> Set a -> Bool
isSubsetOf (Set xs) (Set ys) = isSubsetOf' xs ys 
    where isSubsetOf' []     _ = True
          isSubsetOf' (x:xs) ys = 
                if elem x ys then isSubsetOf' xs ys
                             else False

isProperSubsetOf ::Eq a => Set a -> Set a -> Bool
isProperSubsetOf xs ys = isSubsetOf xs ys && (not (isSubsetOf ys xs))

disjoint :: Eq a => Set a -> Set a -> Bool
disjoint (Set xs) (Set ys) = L.intersect xs ys == []

pairwiseDisjoint :: Set (Set a) -> Bool
pairwiseDisjoint = undefined

union :: Eq a => Set a -> Set a -> Set a
union (Set xs) (Set ys) = set $ L.union xs ys

inter :: Eq a => Set a -> Set a -> Set a
inter (Set xs) (Set ys) = set $ L.intersect xs ys

-- relative complement (set difference)
setminus :: Set a -> Set a -> Set a
setminus (Set xs) (Set ys) = undefined

(\\) = setminus
infixr 5 \\

unions :: Set (Set a) -> Set a
unions = undefined

inters :: Set (Set a) -> Set a
inters = undefined

cartesianProduct :: Set a -> Set b -> Set (a, b)
cartesianProduct = undefined

disjointUnion :: Set a -> Set b -> Set (Either a b)
disjointUnion = undefined

filter :: (a -> Bool) -> Set a -> Set a
filter = undefined

partition :: (a -> Bool) -> Set a -> (Set a, Set a)
partition = undefined

map :: (a -> b) -> Set a -> Set b
map = undefined

