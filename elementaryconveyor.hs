module ElementaryConveyor (pathCounter) where
import Data.Function (on)
import Data.List (groupBy, find)
import Data.Maybe (fromJust)
import Data.Array

pathCounter :: [String] -> [[Int]]
pathCounter b = array2DToListOfLists $ defaultBoard // flood [initialSpot] 0
  where
    m = length b
    n = length (head b)
    defaultBoard = listArray ((0,0), (m-1, n-1)) (repeat (-1)) 
    board = array ((0, 0), (m-1, n-1)) [((i, j), b !! i !! j) | i <- [0..m-1], j <- [0..n-1]]
    initialSpot = fst . fromJust . find ((== 'f') . snd) . assocs $ board
    array2DToListOfLists = map (map snd) . groupBy ((==) `on` (fst . fst)) . assocs
    flood [] _ = []
    flood spots n = ((, n) <$> spots) ++ flood (concatMap nextSpots spots) (n+1)
    nextSpots spot = map snd . filter fst $ zip (pointsToSpot spot) (neighborsUDLR spot)
    pointsToSpot = map (uncurry (==)) . zip "udlr" . map (board !) . neighborsUDLR
    neighborsUDLR (x, y) = [((x+1) `mod` m, y), ((x-1) `mod` m, y), (x, (y+1) `mod` n), (x, (y-1) `mod` n)]
    
