module Count (count) where
import qualified Data.Map as M

count :: String -> [(Char,Int)]
count = M.toList . M.fromListWith (+) . map (\c -> (c, 1))
