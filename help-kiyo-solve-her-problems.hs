module Kata (kiyoLcm) where
import Data.Either (rights)

kiyoLcm :: [[Either Char Int]] -> Int
kiyoLcm = foldl1 lcm . map (sum . filter odd . rights)
