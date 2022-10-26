module Func where

class Functor f where
    fmap :: (a -> b) -> f a -> f b

instance Functor [] where
    fmap = map