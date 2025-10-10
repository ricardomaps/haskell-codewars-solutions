module Codewars.G964.Opstrings1 where
import Data.List (transpose, intercalate, zipWith)
import Control.Applicative ((<*>))

rot90Clock :: [Char] -> [Char]
rot90Clock = intercalate "\n" . map reverse . transpose . lines

diag1Sym :: [Char] -> [Char]
diag1Sym = intercalate "\n" . transpose . lines

selfieAndDiag1 :: [Char] -> [Char]
selfieAndDiag1 = intercalate "\n" . (zipWith (\a b -> a ++ "|" ++ b) <$> lines <*> (lines . diag1Sym)) 

oper :: (String -> String) -> String -> String
oper fct = fct
