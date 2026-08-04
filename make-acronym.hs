module MakeAcronym where
import Data.Char (toUpper)

toAcronym :: String -> String
toAcronym = map (toUpper . head) . words
