module Coproduct where

import Prelude hiding (Either(..))

data Either a b = Left a
                | Right b
                deriving( Show, Eq )

h :: Either a b -> d
h x = case x of
    Left y -> f y
    Right y -> g y
    where
        f :: a -> d
        f = undefined
        g :: b -> d
        g = undefined