module Kata where
import Data.List (intercalate)

expandedForm :: Int -> String
expandedForm = intercalate " + " . reverse . map (\(z, d) -> d : replicate z '0') . filter ((/= '0') . snd) . zip [0..] . reverse . show
