module LookAndSay where
import Data.List (group)

lookSay :: Integer -> Integer
lookSay = undigits 0 . concatMap (\g -> digits (fromIntegral $ length g) ++ g) . group . reverse . digits
  where
    digits !0 = [] 
    digits !n = n `mod` 10 : digits (n `div` 10)

    undigits !n [] = n
    undigits !n (d:ds) = undigits (n * 10 + d) ds
