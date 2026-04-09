module ApplyParentheses (Parentheses, toStr, repr, apply) where
import Control.Applicative (liftA2)
import Text.ParserCombinators.Parsec hiding (State)
import Control.Monad (void)
import Control.Monad.State
import Control.Monad.Trans.Maybe

data Parentheses = Var | Op Parentheses Parentheses deriving Show

toStr :: Parentheses -> String
toStr Var = ""
toStr (Op p1 p2) =  "(" ++ toStr p1 ++ "," ++ toStr p2 ++ ")"

repr :: String -> Maybe Parentheses
repr str = case parse (parentheses <* eof) "" str of
  Right par -> Just par
  Left  _   -> Nothing
  where
  parentheses = op <|> var
  var = return Var
  op = do
    void $ char '('
    left <- parentheses
    void $ char ','
    right <- parentheses
    void $ char ')'
    return $ Op left right

apply :: Parentheses -> (a -> a -> a) -> [a] -> Maybe a
apply p f l = case runState (runMaybeT (go p)) l of
  (Just res, []) -> Just res
  _              -> Nothing
  where
  go Var = do
    elems <- get
    case elems of
      [] -> MaybeT . pure $ Nothing
      (x:xs) -> put xs >> return x
  go (Op l r) = do
    left <- go l
    right <- go r
    return (f left right)
