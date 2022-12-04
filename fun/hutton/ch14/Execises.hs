module Exercises where

import Data.Monoid
import Data.Foldable

-- 1.
-- instance (Monoid a, Monoid b) => Monoid (a,b) where
--     -- mempty :: (a,b)
--     mempty = (mempty, mempty)

--     -- mappend :: (a,b) -> (a,b) -> (a,b)
--     -- (x1,y1) `mappend` (x2,y2) = (x1 `mappend` x2, y1 `mappend` y2)



-- 2.
-- instance (Monoid b) => Monoid (a -> b) where
--     -- mempty :: (a -> b)
--     mempty = \x -> b

--     -- mappend :: (a -> b) -> (a -> b) -> (a -> b)
--     mappend f g = \x -> f x `mappend` g x 



-- 3.
-- instance Foldable Maybe where
--     -- fold :: Monoid a => (Maybe a) -> a
--     fold Nothing  = mempty  
--     fold (Just x) = x

--     -- foldMap :: Monoid b => (a -> b) -> Maybe a -> b
--     foldMap _ Nothing  = mempty
--     foldMap f (Just x) = f x

--     -- foldr :: (a -> b -> b) -> b -> Maybe a -> b
--     foldr _ _ Nothing  = mempty
--     foldr f v (Just x) = f x v

--     foldl :: (b -> a -> b) -> b -> Maybe a -> b
--     foldl f _ Nothing  = mempty
--     foldl f v (Just x) = f v x

-- instance Traversable Maybe where
    -- traverse :: Applicative f => (a -> f b) -> Maybe a -> f (Maybe b)
    -- traverse g Nothing  = pure Nothing  
    -- traverse g (Just x) = fmap Just (g x)  



-- 4. 
data Tree a = Leaf | Node (Tree a) a (Tree a)
    deriving (Show, Functor)

instance Foldable Tree where
    -- fold :: Monoid a => Tree a -> a
    fold Leaf           = mempty
    fold (Node tl x tr) = fold tl <> x <> fold tr

    -- foldMap :: Monoid b => (a -> b) -> Tree a -> Tree b
    foldMap _ Leaf           = mempty
    foldMap f (Node tl x tr) = foldMap f tl <> f x <> foldMap f tr

instance Traversable Tree where
    -- traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
    traverse g Leaf           = pure Leaf
    traverse g (Node tl x tr) = pure Node <*> traverse g tl <*> g x <*> traverse g tr
    -- tl :: Tree a
    -- tr :: Tree a
    -- x  :: a
    -- g  :: a -> f b
    -- Node :: Tree a -> a -> Tree a
    -- pure Node :: f (Tree b -> b -> Tree b -> Tree b)
    -- traverse g tl :: f (Tree b)
    -- traverse g tl :: f (Tree b)
    -- g x :: f b



-- 5.
filterF :: Foldable t => (a -> Bool) -> t a -> [a]
filterF p ts = foldMap (\x -> if p x then [x] else []) ts


