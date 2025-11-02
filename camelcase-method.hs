module CamelCase.JorgeVS.Kata where
import Data.Char (toUpper)

camelCase :: String -> String
camelCase = concatMap capitalize . words
  where capitalize (l:w) = toUpper l : w
