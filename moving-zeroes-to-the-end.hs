module MovingZeros (moveZeros) where
import Data.List (partition)

moveZeros :: [Int] -> [Int]
moveZeros = uncurry (++) . partition (/= 0)
