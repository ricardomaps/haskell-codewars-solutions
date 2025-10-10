module TheBee (theBee) where
import Data.Vector ((!), (!?), generate)

theBee :: Integer -> Integer
theBee m = dp ! (2*n-2) ! (n-1)
  where n = fromIntegral m
        dp = generate (2*n-1) f
        f 0 = generate n (const (fromIntegral 1))
        f i = let prevSize = length (dp ! (i-1))
                  currSize = if i >= n then prevSize-1 else prevSize+1
              in generate currSize (g i)
        g i j = if i >= n 
                then attempt (i-1) j + attempt (i-1) (j+1) + attempt i (j-1) 
                else attempt (i-1) j + attempt (i-1) (j-1) + attempt i (j-1)
        attempt i j = case dp ! i !? j of
                        Nothing -> (fromIntegral 0)
                        Just val -> val
