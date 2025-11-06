{-# LANGUAGE LambdaCase #-}
module ApplicativeParser where

import Data.Char
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)
import Prelude hiding (fmap)

-- | An ambiguous parser.
newtype Parser a = P { unP :: String -> [(String, a)] }

-- | Change the result of a parser.
pmap :: (a -> b) -> Parser a -> Parser b
pmap f (P p) = P (map (second f) . p) 
  where second g (x, y) = (x, g y)

-- | Operator version of 'pmap'.
(<#>) :: (a -> b) -> Parser a -> Parser b
(<#>) = pmap

-- | Parse a value and replace it.
(<#) :: a -> Parser b -> Parser a
(<#) x = pmap (const x)

infixl 4 <#>
infixl 4 <#

-- | Parse a character only when a predicate matches.
predP :: (Char -> Bool) -> Parser Char
predP p = P $ \case
  ""    -> []
  (c:s) -> [(s, c) | p c]

-- | Succeed only when parsing the given character.
charP :: Char -> Parser Char
charP = predP . (==)

-- | Inject a value into an identity parser.
inject :: a -> Parser a
inject x = P (\s -> [(s, x)])

-- | Given a parser with a function value and another parser, parse the function
-- first and then the value, return a parser which applies the function to the
-- value.
-- Parser (String -> [(String, a -> b)]) -> Parser (String -> [(String, a)])
(<@>) :: Parser (a -> b) -> Parser a -> Parser b
pf <@> px = P $ \s -> do
  (s1, f) <- unP pf s
  (s2, x) <- unP px s1
  return (s2, f x)

(<@) :: Parser a -> Parser b -> Parser a
pa <@ pb = const <#> pa <@> pb

(@>) :: Parser a -> Parser b -> Parser b
pa @> pb = const id <#> pa <@> pb

infixl 4 <@
infixl 4 @>
infixl 4 <@>

-- | Parse a whole string.
stringP :: String -> Parser String
stringP "" = inject ""
stringP (c:s) = (:) <#> charP c <@> stringP s

-- | Construct a parser that never parses anything.
emptyP :: Parser a
emptyP = P (const [])

-- | Combine two parsers: When given an input, provide the results of both parser run on the input.
(<<>>) :: Parser a -> Parser a -> Parser a
p1 <<>> p2 = P (\s -> unP p1 s ++ unP p2 s)

infixl 3 <<>>

-- | Apply the parser zero or more times.
many :: Parser a -> Parser [a]
many p = some p <<>> inject []

-- | Apply the parser one or more times.
some :: Parser a -> Parser [a]
some p = (:) <#> p <@> many p


-- | Apply a parser and return all ambiguous results, but only those where the input was fully consumed.
runParser :: Parser a -> String -> [a]
runParser p cs = map snd . filter (null . fst) $ unP p cs

-- | Apply a parser and only return a result, if there was only one unambiguous result with output fully consumed.
runParserUnique :: Parser a -> String -> Maybe a
runParserUnique p cs =
  case runParser p cs of
    [x] -> Just x
    _   -> Nothing

-- | Kinds of binary operators.
data BinOp = AddBO | MulBO deriving (Eq, Show)

-- | Some kind of arithmetic expression.
data Expr = ConstE Int
          | BinOpE BinOp Expr Expr
          | NegE Expr
          | ZeroE
          deriving (Eq, Show)

evalExpr :: Expr -> Int
evalExpr (ConstE n) = n
evalExpr (BinOpE AddBO e1 e2) = evalExpr e1 + evalExpr e2
evalExpr (BinOpE MulBO e1 e2) = evalExpr e1 * evalExpr e2
evalExpr (NegE e) = negate (evalExpr e)
evalExpr ZeroE = 0


-- | Parse arithmetic expressions, with the following grammar:
--
--     expr         ::= const | binOpExpr | neg | zero
--     const        ::= int
--     binOpExpr    ::= '(' expr ' ' binOp ' ' expr ')'
--     binOp        ::= '+' | '*'
--     neg          ::= '-' expr
--     zero         ::= 'z'
-- 
parseExpr :: String -> Maybe Expr
parseExpr = runParserUnique exprP
  where
  exprP = constP <<>> binOpExprP <<>> negP <<>> zeroP
  constP = ConstE . read <#> some (predP isDigit)
  binOpExprP =
    charP '(' @> (flip BinOpE <#> exprP) <@ charP ' '
    <@> binOpP
    <@> (charP ' ' @> exprP <@ charP ')')
  binOpP = (charP '+' @> inject AddBO) <<>> (charP '*' @> inject MulBO)
  negP = NegE <#> (charP '-' @> exprP)
  zeroP = inject ZeroE <@ charP 'z'
