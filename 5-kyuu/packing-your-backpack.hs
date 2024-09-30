module PackingYourBackpack (packBackpack) where
import Data.Vector (generate, fromList, (!))

packBackpack :: [Int] -> [Int] -> Int -> Int
packBackpack [] _ _ = 0
packBackpack _ _ 0 = 0
packBackpack scores weights capacity = dp!(length items - 1)!capacity
  where 
    items = zip3 [0..] scores weights
    dp = fromList (map f items)
    f item = generate (capacity+1) (g item)
    g (i, score, weight) cap
      | i == 0 = if weight <= cap then score else 0
      | weight > cap = dp!(i-1)!cap
      | otherwise = max (dp!(i-1)!cap) (score + dp!(i-1)!(cap-weight))
