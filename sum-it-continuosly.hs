module Haskell.SylarDoom.SumContinuously where
import Data.List(scanl1)

add :: [Integer] -> [Integer]
add = scanl1 (+)
