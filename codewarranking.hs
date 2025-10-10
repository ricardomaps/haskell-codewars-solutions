module CodewarRanking where

data User = User { rank :: Int, progress :: Int }

newUser :: User
newUser = User (-8) 0

incProgress :: Int -> User -> User
incProgress kataRank user
  | kataRank < rank user - 1  = user
  | kataRank == rank user - 1 = updateRank $ user { progress = progress user + 1}
  | kataRank == rank user     = updateRank $ user { progress = progress user + 3}
  | kataRank > rank user      = updateRank $ user { progress = progress user + 10 * (kataRank - rank user) ^ 2  }
  | kataRank < (-8) || kataRank > 8 || kataRank == 0 = error "thats not real bruh"
  where
    updateRank user
      | rank user == 8 = user { progress = max (progress user) 100 }
      | progress user > 100 =
          user { progress = progress user `mod` 100
               , rank = let addedRanks = progress user `div` 100 
                        in rank user + addedRanks + if rank user >= (-1) * addedRanks then 1 else 0
               }
      | otherwise = user
