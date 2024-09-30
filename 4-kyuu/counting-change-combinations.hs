module Change where
import Data.Vector (generate, (!), fromList)

countChange :: Integer -> [Integer] -> Integer
countChange 0 _ = 1
countChange _ [] = 0
countChange n (coin:coins) = sum [countChange m coins | m <- [n, n-coin .. 0]]
