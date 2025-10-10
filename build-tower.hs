module Codewars.BuildTower where

buildTower :: Int -> [String]
buildTower n = 
  map build [0..n-1]
  where
  build i = replicate (spaces i) ' ' ++ replicate (asterisks i) '*' ++ replicate (spaces i) ' '
  asterisks i = 2 * i + 1
  spaces i = n - 1 - i

