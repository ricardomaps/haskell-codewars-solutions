module LookAndSay (getLines) where
import Data.List (group, intercalate)
import Control.Applicative (liftA2)

las :: [String]
las = "1" : map (concat . liftA2 (zipWith (++)) (map (show . length)) (map (take 1)) . group) las

getLines :: Int -> String
getLines = intercalate "," . flip take las
