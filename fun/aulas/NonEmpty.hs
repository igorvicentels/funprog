module NonEmpty where

data NonEmpty a = Singleton a 
                | Cons a (NonEmpty a)

-- data NonEmpty a = Cons a [a]

-- Versão do haskell
-- data NonEmpty a = a :| [a]