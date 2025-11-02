module FreqSort where 
import Data.List (sort, sortOn, group)
import Data.Function (on)

solve :: [Int] -> [Int]
solve = concat . sortOn (negate . length) . group . sort
