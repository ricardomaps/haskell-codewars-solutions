module FixArraySequence (solve) where
import Data.List (sortOn, sort)
import Data.Bits (countTrailingZeros)

solve :: [Integer] -> [Integer]
solve = bracket (sortOn countTrailingZeros . reverse . sort)
  where
  bracket :: ([Int] -> [Int]) -> [Integer] -> [Integer]
  bracket f = map fromIntegral . f . map fromIntegral 
