module SimpleTokenizer (Token(..), tokenize) where
import Text.ParserCombinators.ReadP as Parser
import Data.Char (isLetter)
import Control.Applicative ((<|>))

data Token = Token String | Brackets [Token]
  deriving (Eq, Show)

tokenize :: String -> Maybe [Token]
tokenize input =
  case readP_to_S (many parseToken <* eof) input of
    [(result, _)] -> Just result
    _             -> Nothing

operatorChars :: String
operatorChars = "!#$%&*+-/<=>@^_.,;"

parseOperator :: ReadP Token
parseOperator = Token <$> munch1 (`elem` operatorChars)

parseIdentifier :: ReadP Token
parseIdentifier = Token <$> munch1 isLetter

parseBracket :: ReadP Token
parseBracket = do
  char '('
  inner <- many parseToken
  char ')'
  return (Brackets inner)

parseToken :: ReadP Token
parseToken = skipSpaces >> (parseIdentifier <|> parseOperator <|> parseBracket)
