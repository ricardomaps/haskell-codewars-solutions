module NBack (countTargets) where
import Control.Applicative

countTargets :: (Eq a) => Int -> [a] -> Int
countTargets n = length . filter id . (zipWith (==) <*> drop n)
