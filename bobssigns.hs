module BobsSigns (estimate) where
import Data.Vector ((!), generate)

estimate :: Int -> Int -> String -> String -> Int
estimate add remove s1 s2 = dp ! m ! n
  where
    m = length s1
    n = length s2
    dp = generate (m+1) (\i -> generate (n+1) (\j -> 
      if i == 0 then add * j
      else if j == 0 then remove * i
      else if s1 !! (i-1) ==  s2 !! (j-1) then
        dp ! (i-1) ! (j-1)
      else
        min (add + dp ! i ! (j-1)) (remove + dp ! (i-1) ! j)))
