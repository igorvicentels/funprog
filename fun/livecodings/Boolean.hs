module Boolean where

data Boolean = F | T deriving (Show)

lor :: Boolean -> Boolean -> Boolean
lor F b = b
lor T _ = T

land :: Boolean -> Boolean -> Boolean
land T b = b
land F _ = F
