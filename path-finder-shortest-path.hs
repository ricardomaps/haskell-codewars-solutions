module ShortestPath (pathFinder) where
import qualified Data.Set as S
import qualified Data.Array as A

pathFinder :: String -> Maybe Int
pathFinder str = go (S.singleton (0, 0)) [(0, 0)] 0
  where
  rows = lines str
  sideLength = length rows
  board =
    A.array
      ((0, 0), (sideLength-1, sideLength-1))
      [((i, j), rows !! i !! j) | i <- [0..sideLength-1], j <- [0..sideLength-1]]
  unvisitedNeighbors (i, j) visited =
    [(x, y) |
      (x, y) <- [(i-1, j), (i, j-1), (i+1, j), (i, j+1)],
      x  >= 0 && y >= 0 && x < sideLength && y < sideLength,
      board A.! (x, y) /= 'W',
      (x, y) `S.notMember` visited]
  go visited cur steps
    | null cur = Nothing
    | (sideLength - 1, sideLength - 1) `elem` cur = Just steps
    | otherwise =
        let (visited', cur') = foldr step (visited, []) cur
        in go visited' cur' (steps + 1)
  step pos (vis, next) =
    let new  = unvisitedNeighbors pos vis
        vis' = foldr S.insert vis new
    in (vis', next ++ new)
