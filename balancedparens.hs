module Balanced.Parens where

balancedParens :: Int -> [String]
balancedParens 0 = [""]
balancedParens n = do
  m <- [0..n-1]
  inside  <- balancedParens m
  outside <- balancedParens (n-m-1)
  return $ "(" <> inside <> ")" <> outside

-- balancedParens 0 -> [""]
-- balancedParens 1 -> ["()"]
-- balancedParens 2 -> ["()()","(())"]
-- balancedParens 3 -> ["()()()","(())()","()(())","(()())","((()))"]
