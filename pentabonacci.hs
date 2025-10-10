module Codewars.Kata.Pentabonacci where

countOddPentaFib :: Int -> Int
countOddPentaFib n = (n `div` 6) * 2 + min (n `mod` 6) 2 - if n > 1 then 1 else 0


-- f(0) = 0 even
-- f(1) = 1 odd
-- f(2) = 1 odd
-- f(3) = 2 even
-- f(4) = 4 even
-- f(n) = even
-- f(6) = even
-- f(7) = odd
-- f(8) = odd
