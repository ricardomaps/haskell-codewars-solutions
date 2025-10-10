module DuplicateArguments (solution) where
import Data.List (nub)

solution :: (Eq a,Ord a) => [a] -> Bool
solution = (/=) <*> nub
