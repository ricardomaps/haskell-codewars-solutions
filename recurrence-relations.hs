module FunctionEvaluator where
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map (Map)

evaluateFunction :: Ord a => (a -> Either b ([a], [b] -> b)) -> a -> b
evaluateFunction f n = evalState (go n) (Map.empty)
  where
  go n = do
    m <- get 
    case Map.lookup n m of
      Just v -> return v
      Nothing ->
        case f n of
          Left base -> modify' (Map.insert n base) >> return base
          Right (rec, f') -> do
            res <- mapM go rec
            let !v = f' res
            modify' (Map.insert n v)
            return v
