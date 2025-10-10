module SortArray where
import Data.List (sort)
import Control.Arrow
import qualified Control.Category as Cat

newtype SF a b = SF {runSF :: [a] -> [b]}

instance Cat.Category SF where
  id          = SF (map id)
  SF f . SF g = SF (f . g)
  
instance Arrow SF where
  arr          = SF . map
  SF f *** SF g = SF (\l -> let (xs, ys) = unzip l in zip (f xs) (g ys))

instance ArrowChoice SF where
  left (SF f) = SF (\xs -> combine xs (f [y | Left y <- xs]))
    where combine (Left _:xs)  (z:zs) = Left z  : combine xs zs
          combine (Right x:xs) zs     = Right x : combine xs zs
          combine []           _     = []

sortArray :: [Int] -> [Int]
sortArray = runSF $ arr tag >>> SF sort ||| arr id
  where tag x = if odd x then Left x else Right x
