module Codewars.Kata.Bookseller where
import Codewars.Kata.Bookseller.Types
import qualified Data.Map as Map

-- data Stock    = Stock String Int deriving (Show, Eq)
stocklist :: [Stock] -> [Char] -> [(Char, Int)]
stocklist [] _ = []
stocklist _ [] = []
stocklist st cs = map findBooks cs
  where stock = Map.fromListWith (+) $ map (\(Stock code quant) -> (head code, quant)) st
        findBooks c = if Map.member c stock then (c, stock Map.! c) else (c, 0)
  
