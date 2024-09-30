module Codewars.Kata.DecodeMorse (decodeMorse) where
import Codewars.Kata.DecodeMorse.Preload (morseCodes)
import Data.Map.Strict ((!))
import Data.List.Split

decodeMorse :: String -> String
decodeMorse =
  unwords . filter (not . null) . map (concatMap (morseCodes !) . words) . splitOn "   "
