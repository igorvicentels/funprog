module Person where

import Prelude hiding ( Either(..) )

type Name = String
type Age = Int

data Person = Person Name Age
    deriving (Show)

data Either a b = Left a
                | Right b
                deriving ( Show, Eq )

data PersonError = NameError
                 | AgeError
                 deriving ( Show, Eq )
person :: Name -> Age -> Either PersonError Person
person name age
    | length name < 2 = Left NameError
    | age > 200       = Left AgeError
    | otherwise       = Right $ Person name age