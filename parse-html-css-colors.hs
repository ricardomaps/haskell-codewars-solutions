module Codewars.Lambda4fun.ParseHtmlColor where

import Codewars.Lambda4fun.ParseHtmlColor.PresetColors (presetColors)
import Data.Map.Strict (Map, fromList, (!))
import Numeric (readHex)
import Data.List.Split (chunksOf)
import Data.Char (toLower)

parseHtmlColor :: String -> Map Char Int
parseHtmlColor s 
  | head s == '#' = if length s == 7 then parseHex (tail s) else parseHex (to6Hex $ tail s)
  | otherwise = parseHex (tail $ presetColors ! (map toLower s))
  where parseHex = fromList . zip "rgb" . map (fst . head . readHex) . chunksOf 2
        to6Hex [] = []
        to6Hex (hd:tl) = hd : hd : to6Hex tl
