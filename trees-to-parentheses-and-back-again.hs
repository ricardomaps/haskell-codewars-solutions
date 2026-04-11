module TreesToParensSolution where
import Text.ParserCombinators.Parsec
import Control.Monad (void)
import Data.Either (fromRight)

-- import TreesToParensPreloaded (Tree(..))

data Tree = Leaf | Tree :*: Tree deriving (Eq, Show)

treeToParens :: Tree -> String
treeToParens tree = go tree ""
  where
  go    Leaf   acc = acc
  go (l :*: r) acc = '(' : go l (')' : go r acc)

parensToTree :: String -> Tree
parensToTree = fromRight (error "not supposed to happen") . parse parseTree  ""
  where
  parseTree = parseBranch <|> parseLeaf
  parseLeaf = return Leaf
  parseBranch = do
    void $ char '('
    l <- parseTree
    void $ char ')'
    r <- parseTree
    return $ l :*: r
