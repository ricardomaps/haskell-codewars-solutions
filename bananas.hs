module Bananas (bananas) where

bananas :: String -> [String]
bananas = go "banana"
  where
  go [] r = ['-' <$ r]
  go _ [] = []
  go p@(c1:s1) (c2:s2) =
    let take = if c1 == c2 then (c1:) <$> go s1 s2 else []
        notTake = ('-':) <$> go p s2 
    in take ++ notTake
