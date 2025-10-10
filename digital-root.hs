module DigitalRoot where
import Data.Char (digitToInt)

digitalRoot :: Integral a => a -> a
digitalRoot n
  | digSum >= 10 = digitalRoot digSum
  | otherwise = digSum
  where digSum = fromInteger . sum . map (toInteger . digitToInt) . show . toInteger $ n
