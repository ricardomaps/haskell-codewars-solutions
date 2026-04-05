module Kata where
import Data.Ord (comparing)

data Shape =
    Square      { side :: Double }
  | Rectangle   { width :: Double, height :: Double }
  | Triangle    { base :: Double, height :: Double }
  | Circle      { radius :: Double }
  | CustomShape { area :: Double }
  deriving (Show, Eq)

instance Ord Shape where
  compare = comparing getArea

getArea :: Shape -> Double
getArea shape = case shape of
  Square { side }             -> side * side 
  Rectangle { width, height } -> width * height
  Triangle { base, height }   -> (base * height) / 2
  Circle { radius }           -> pi * radius * radius
  CustomShape { area }        -> area
  




