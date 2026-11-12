module PairOfGloves (numberOfPairs) where
import qualified Data.Map as Map

data Colour = Red | Blue | Green deriving (Show,Eq,Ord,Enum,Bounded)

numberOfPairs :: [Colour] -> Int
numberOfPairs = Map.foldr (\n acc -> n `div` 2 + acc) 0 . Map.fromListWith (+) . flip zip (repeat 1)
