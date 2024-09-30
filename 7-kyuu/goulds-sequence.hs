module Gould (gould) where
import Data.Bits (popCount)

gould :: [Int]
gould = go 0
  where
  go :: Int -> [Int]
  go n = popCount n : go (succ n)
