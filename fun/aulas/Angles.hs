module Angles where

newtype Angle = Angle Int

instance Eq Angle where
    Angle x == Angle y = 360 `divides` (x - y)

divides :: Int -> Int -> Bool
divides x y = y `mod` x == 0

isTriangle :: Angle -> Angle -> Angle -> Bool
isTriangle (Angle x) (Angle y) (Angle z) = sum [x,y,z] == 180

casamento :: Angle -> String
casamento (Angle 2) = "foi o 2"
casamento (Angle _) = "oi"

bottomAngle :: Angle
bottomAngle = bottomAngle

bottomInt :: Int
bottomInt = bottomInt
