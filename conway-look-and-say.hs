module LookAndSay where
import Data.List (group)

lookSay :: Integer -> Integer
lookSay = read . concatMap (\ds -> show (length ds) ++ [head ds]) . group . show
