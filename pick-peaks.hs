module PickPeak.JorgeVS.Kata where
data PickedPeaks = PickedPeaks { pos :: [Int], peaks :: [Int]} deriving (Eq, Show)
pickPeaks :: [Int] -> PickedPeaks
pickPeaks = lowerThanPrev initPeaks . zip [0..]
  where
  initPeaks = PickedPeaks { pos = [], peaks = [] }
  higherThanPrev :: PickedPeaks -> [(Int, Int)] -> PickedPeaks
  higherThanPrev picks (x1@(pos1, val1):xs@((_, val2):_))
    | val1 < val2 = higherThanPrev picks xs
    | val1 == val2 = higherThanPrev picks $ x1:(dropWhile ((== val1) . snd) xs)
    | otherwise = lowerThanPrev (picks {pos = pos picks ++ [pos1], peaks = peaks picks ++ [val1]}) xs
  higherThanPrev peaks _ = peaks
  lowerThanPrev :: PickedPeaks -> [(Int, Int)] -> PickedPeaks
  lowerThanPrev peaks ((_, val1):xs@((_, val2):_))
    | val1 < val2 = higherThanPrev peaks xs
    | otherwise = lowerThanPrev peaks xs
  lowerThanPrev peaks _ = peaks
