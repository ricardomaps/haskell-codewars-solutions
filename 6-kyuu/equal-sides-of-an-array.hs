module Codewars.G964.FindEven where
import Data.Maybe (fromMaybe)
import Data.List (elemIndex)

findEvenIndex :: [Int] -> Int
findEvenIndex arr = fromMaybe (-1) . elemIndex True $ zipWith (==) (scanl1 (+) arr) (scanr1 (+) arr)
