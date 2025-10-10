module Codewars.G964.Partlist where
import Data.List

partlist :: [String] -> [(String, String)]
partlist = tail . init . (zip <$> (map unwords . inits) <*> (map unwords . tails))
