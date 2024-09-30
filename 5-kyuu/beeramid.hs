module Beer where
import Data.List (scanl1)

beeramid :: Double -> Double -> Int
beeramid bonus price = length . takeWhile ((<= bonus)) . scanl1 (+) . map ((* price). (^2)) $ [1..]
