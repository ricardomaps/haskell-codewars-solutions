module JSON.Parser (parse) where
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding (parse, string)
import Control.Monad (void)
import Prelude hiding (null)

data Value = String String
           | Number Double
           | Object [(Value,Value)] -- an association list -- only a `String` is valid as the index `Value`
           | Array [Value]          -- not limited to identical primitive datatypes
           | Boolean Bool           -- either `True` or `False`
           | Null
           deriving Show

parse :: String -> Maybe Value
parse = eitherToMaybe . P.parse (jsonValue <* eof) ""
  where
  eitherToMaybe (Right x) = Just x
  eitherToMaybe (Left _)  = Nothing

string :: Parser Value
string = String <$> betweenQuotes (many notQuote)
  where
  notQuote = satisfy (/= '"')
  betweenQuotes = lexeme . between (char '"') (char '"')

number :: Parser Value
number = lexeme $ do
  sign <- option "" (P.string "-")
  int <- P.string "0" <|> ((:) <$> oneOf ['1'..'9'] <*> many digit)
  frac <- option "" ((:) <$> char '.' <*> many1 digit)
  return $ Number $ read (sign ++ int ++ frac)

object :: Parser Value
object = Object <$> betweenCurlyBrackets members
  where
  betweenCurlyBrackets = between (lexeme $ char '{') (lexeme $ char '}') 
  members = pair `sepBy` char ',' 
  pair = do
    key <- string
    void $ lexeme $ char ':'
    value <- jsonValue
    return (key, value)

array :: Parser Value
array = Array <$> betweenSquareBrackets (jsonValue `sepBy` char ',')
  where
  betweenSquareBrackets = between (lexeme $ char '[') (lexeme $ char ']') 

boolean :: Parser Value
boolean = Boolean <$> (true <|> false)
  where
  true = True <$ (lexeme $ P.string "true")
  false = False <$ (lexeme $ P.string "false")

null :: Parser Value
null = Null <$ (lexeme $ P.string "null")

jsonValue :: Parser Value
jsonValue = spaces *> choice [number, string, boolean, null, array, object]

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces
