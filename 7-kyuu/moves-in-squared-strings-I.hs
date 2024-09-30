module Codewars.G964.Opstrings1 where
import Data.List (intercalate)

vertMirror :: [Char] -> [Char]
vertMirror = intercalate "\n" . map reverse . lines

horMirror :: [Char] -> [Char]
horMirror = intercalate "\n" . reverse . lines

oper :: (String -> String) -> String -> String
oper = id
