module Records where

type Name = String

data Sex = Male | Female
    deriving ( Show, Eq )

type Age = Int

data Person = Person { name :: Name
                     , sex :: Sex
                     , age :: Age
                     }
    deriving ( Show )


-- getters
-- name :: Person -> Name
-- name (Person n _ _) = n

-- sex :: Person -> Sex
-- sex (Person _ s _) = s

-- age :: Person -> Age
-- age (Person _ _ a) = a

-- updateName :: Person -> Name -> Person
-- updateName (Person _ s a) n = Person n s a 