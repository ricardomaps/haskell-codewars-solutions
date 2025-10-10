module TreeTop where

data Tree a = Nil | Node (Tree a) a (Tree a) deriving (Show)

treeTop :: Tree Int -> [Int]
treeTop Nil          = []
treeTop (Node l x r) = reverse (goLeft l) ++ [x] ++ goRight r
  where
    goLeft Nil           = []
    goLeft (Node l x _)  = x : goLeft l
    goRight Nil          = []
    goRight (Node _ x r) = x : goRight r
