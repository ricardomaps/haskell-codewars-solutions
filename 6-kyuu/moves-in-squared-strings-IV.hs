module Codewars.G964.Opstrings1 where
import Data.List
import Control.Applicative

withLines :: ([String] -> [String]) -> (String -> String)
withLines f = intercalate "\n" . f . lines

rot90Counter :: [Char] -> [Char]
rot90Counter = withLines $ reverse . transpose

diag2Sym :: [Char] -> [Char]
diag2Sym = withLines $  reverse . transpose . reverse

join :: String -> String -> String -> String
join a b c = intercalate "|" [a, b, c]

selfieDiag2Counterclock :: [Char] -> [Char]
selfieDiag2Counterclock = withLines $ zipWith3 join <*> (reverse . transpose . reverse) <*> (reverse . transpose)

oper :: (String -> String) -> String -> String
oper = ($)

