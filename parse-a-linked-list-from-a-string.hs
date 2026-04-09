module ParseListFromString (parse) where
import Data.List.Split (splitOn)

parse :: String -> [Word]
parse = map read . init . splitOn "->"

