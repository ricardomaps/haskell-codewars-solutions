module Validate where
import Data.Function ((&))
import Data.Char (digitToInt)

validate :: Integer -> Bool
validate n =
  show n
  & map digitToInt
  & reverse
  & zipWith (\i n -> if odd i then 2 * n else n) [0..]
  & map (\n -> if n > 9 then n - 9 else n)
  & sum
  & \n -> n `mod` 10 == 0
