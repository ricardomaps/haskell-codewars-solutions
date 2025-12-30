module BlackJack where 

scoreHand :: [String] -> Int
scoreHand xs = points + aceScoring
  where
  (points, aces) = foldr score (0, 0) xs
  aceScoring
    | aces == 0          = 0
    | aces > 11 - points = aces
    | otherwise          = 10 + aces
  score card (points, aces)
    | card `elem` ["K", "Q", "J"] = (points + 10, aces)
    | card == "A"                 = (points, aces + 1)
    | otherwise                   = (points + read card, aces)
