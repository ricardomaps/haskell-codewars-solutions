module LastDigit (lastDigit) where

cycles :: [[Integer]]
cycles = [[0, 0, 0, 0], [1, 1, 1, 1], [6, 2, 4, 8],
          [1, 3, 9, 7], [6, 4, 6, 4],
          [5, 5, 5, 5], [6, 6, 6, 6], [1, 7, 9, 3],
          [6, 8, 4, 2], [1, 9, 1, 9]]

lastDigitSimple :: Integer -> Integer -> Integer
lastDigitSimple _ 0 = 1 
lastDigitSimple a b = cycles !! (fromIntegral a `mod` 10) !! (fromIntegral b `mod` 4)

lastDigit :: [Integer] -> Integer
lastDigit = foldr lastDigitSimple 1
