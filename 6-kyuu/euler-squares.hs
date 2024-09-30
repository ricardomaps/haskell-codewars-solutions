module EulerSquares where
import Data.List (cycle, iterate)

-- | Assume n is odd
createEulerSquare :: Int -> ([[Int]], [[Int]])
createEulerSquare n = 
  (gen rotateLeft (n:[1..n-1]),  gen rotateRight (n:[n-1, n-2..1]))
  where
  rotateLeft = take n . drop (n-1) . cycle
  rotateRight = take n . drop 1 . cycle
  gen f = take n . iterate f

