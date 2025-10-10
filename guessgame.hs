module GuessGame (guess) where

guess :: Monad m => (Int -> m Bool) -> m Int
guess gt = go 1 100
  where
  go low high
    | low == high = return low
    | otherwise = do
        let mid = (low+high) `div` 2
        greaterThan <- gt mid
        if greaterThan then go (mid+1) high else go low mid
    
