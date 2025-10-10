module LispLovesMe where

data AST = I32 Int
         | Sym String
         | Nul
         | Err
         | Lst [AST]
         | Boo Bool
         | Nod AST [AST]
         deriving (Eq, Show)
--

preludeFunctions :: [(String, [AST] -> AST)]
preludeFunctions =
  [ ("+", undefined)
  , ("*", undefined)
  , ("-", undefined)
  , ("/", undefined)
  , ("^", undefined)
  , (">", undefined)
  , ("<", undefined)
  , ("!", undefined)
  , ("list", undefined)
  , ("size", undefined)
  , ("reverse", undefined)
  , ("..", undefined)
  , ("==", undefined)
  , (">=", undefined)
  , ("<=", undefined)
  , ("!=", undefined)
  , ("if", undefined)
  ]

--

lispPretty :: String -> Maybe String
lispPretty s = undefined

lispEval :: String -> Maybe AST
lispEval s = undefined
