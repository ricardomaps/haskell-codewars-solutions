module CardGame where

cardGame :: Int -> Int
cardGame 0 = 0
cardGame 4 = 3
cardGame n | odd n = n - cardGame (n-1)
           | n `mod` 4 == 0 = 1 + cardGame (n-2)
           | otherwise = n - cardGame(n `div` 2)
