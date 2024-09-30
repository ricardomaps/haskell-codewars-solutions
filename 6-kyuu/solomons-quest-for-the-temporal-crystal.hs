module SolomonsQuest (solomonsQuest) where

solomonsQuest :: [(Int,Int,Int)] -> (Int,Int)
solomonsQuest input = 
  move 0 (0, 0) input
  where
  move _ finalPos [] = finalPos
  move layer (x, y) ((layerDelta, direction, posDelta):xs) =
    let newLayer = layer + layerDelta
        delta = 2^newLayer * posDelta in
    case direction of
      0 -> move newLayer (x, y + delta) xs
      1 -> move newLayer (x + delta, y) xs
      2 -> move newLayer (x, y - delta) xs
      3 -> move newLayer (x - delta, y) xs
