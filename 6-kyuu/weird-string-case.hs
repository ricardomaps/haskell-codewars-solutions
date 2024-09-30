module WeIrDStRiNgCaSe where
import Data.Char (toLower, toUpper)

toWeirdCase :: String -> String
toWeirdCase = unwords . map (zipWith ($) (cycle [toUpper, toLower])) . words
