-- | Evaluation module

module HaskellScheme.Evaluator where
import           HaskellScheme.LispTypes
import           Data.Char                      ( toLower )

instance Show LispVal where
  show = showVal

showVal :: LispVal -> String
showVal (String    contents) = "\"" ++ contents ++ "\""
showVal (Atom      name    ) = name
showVal (Number    contents) = show contents
showVal (Bool      True    ) = "#t"
showVal (Bool      False   ) = "#f"
showVal (Float     contents) = show contents
showVal (Character c       ) = [c]
showVal (List      contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList listHead listTail) =
  "(" ++ unwordsList listHead ++ " . " ++ showVal listTail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Atom _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Character _) = val
eval val@(Bool _) = val
eval val@(List []) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args
eval _ = Bool False

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [ ("+"             , numericBinop (+))
  , ("-"             , numericBinop (-))
  , ("*"             , numericBinop (*))
  , ("/"             , numericBinop div)
  , ("mod"           , numericBinop mod)
  , ("quotient"      , numericBinop quot)
  , ("remainder"     , numericBinop rem)
  , ("symbol?"       , isSymbol)
  , ("number?"       , isNumber)
  , ("string?"       , isString)
  , ("character?"    , isCharacter)
  , ("bool?"         , isBool)
  , ("string->symbol", stringToSymbol)
  , ("symbol->string", symbolToString)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n  ) = n
unpackNum (List   [n]) = unpackNum n
unpackNum _            = 0

isSymbol :: [LispVal] -> LispVal
isSymbol []                    = Bool False
isSymbol [(        Atom _)   ] = Bool True
isSymbol ((Atom _) :     rest) = isSymbol rest
isSymbol (_        :     _   ) = Bool False

isNumber :: [LispVal] -> LispVal
isNumber []                        = Bool False
isNumber [(          Number _)   ] = Bool True
isNumber [(          Float  _)   ] = Bool True
isNumber ((Number _) :       rest) = isNumber rest
isNumber ((Float  _) :       rest) = isNumber rest
isNumber (_          :       _   ) = Bool False

isString :: [LispVal] -> LispVal
isString []                        = Bool False
isString [(          String _)   ] = Bool True
isString ((String _) :       rest) = isString rest
isString (_          :       _   ) = Bool False

isCharacter :: [LispVal] -> LispVal
isCharacter []                              = Bool False
isCharacter [(             Character _)   ] = Bool True
isCharacter ((Character _) :          rest) = isCharacter rest
isCharacter (_             :          _   ) = Bool False

isBool :: [LispVal] -> LispVal
isBool []                    = Bool False
isBool [(        Bool _)   ] = Bool True
isBool ((Bool _) :     rest) = isBool rest
isBool (_        :     _   ) = Bool False

stringToSymbol :: [LispVal] -> LispVal
stringToSymbol [(String val)] = Atom $ map toLower val
stringToSymbol _              = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [(Atom val)] = String val
symbolToString _            = Bool False
