module NextBigger (nextBigger) where

nextBigger :: Int -> Int
nextBigger = go [] . reverse . digits
  where
    go _  [] = Nothing
    go [] (x:xs) = go [x] xs
    go xss@(x:xs) (y:ys)
      | y
