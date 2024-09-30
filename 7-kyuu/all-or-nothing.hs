module AllOrNothing (possiblyPerfect) where

possiblyPerfect :: String -> String -> Bool
possiblyPerfect key answers = all id seq || all not seq
  where seq = map (uncurry (==)) . filter ((/= '_') . fst) $ zip key answers
  
