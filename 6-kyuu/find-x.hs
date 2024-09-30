module FindX where 

--optimize this
findX :: Int -> Int    
findX n = n * (sum2N + 2 * sumN)
  where sumN = sum [0..n-1]
        sum2N = sumN + sum [n..2*n-1]
