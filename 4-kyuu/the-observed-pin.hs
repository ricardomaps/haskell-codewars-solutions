module PIN where

getPINs :: String -> [String]
getPINs = sequenceA
          . map adj 
  where adj '1' = "124"
        adj '2' = "1235"
        adj '3' = "236"
        adj '4' = "1457"
        adj '5' = "24568"
        adj '6' = "3569"
        adj '7' = "478"
        adj '8' = "57890"
        adj '9' = "689"
        adj '0' = "80"
