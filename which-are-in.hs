module Codewars.Kata.Which where
import Data.List(sort, isInfixOf, nub)

-- Sorry for the name of the function.
inArray :: [String] -> [String] -> [String]
inArray a1 a2 = nub . sort $ filter (\s1 -> any (\s2 -> s1 `isInfixOf` s2) a2) a1


