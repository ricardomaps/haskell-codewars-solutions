module Kata (landPerimeter) where
import Data.Array
import Data.Function ((&))

landPerimeter :: [String] -> String
landPerimeter arr = "Total land perimeter: " <> show totalPerimeter
  where
  x = length arr
  y = length (head arr)
  grid = listArray (1, x) (listArray (1, y) <$> arr)
  inBounds (i, j) = i >= 1 && j >= 1 && i <= x && j <= y 
  neighbors (i, j) = [(i-1, j), (i, j-1), (i+1, j), (i, j+1)]
  perimeter (i, j) =
    neighbors (i, j)
    & filter (\(x, y) -> not (inBounds (x, y)) || grid ! x ! y == 'O')
    & length
  totalPerimeter = sum $ do
    (i, row)  <- assocs grid
    (j, cell) <- assocs row
    if cell == 'O' then
      return 0
    else
      return $ perimeter (i, j)
