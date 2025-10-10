module FollowThatSpy (findRoutes) where
import Data.List (unfoldr, intercalate, (\\))
import Control.Arrow ((&&&))

findRoutes :: [(String,String)] -> String
findRoutes routes = intercalate ", " $ unfoldr (fmap $ id &&& flip lookup routes) (Just startingPoint)
  where startingPoint = head . uncurry (\\) . unzip $ routes
