-- | Evaluation module

module HaskellScheme.Evaluator where
import           HaskellScheme.LispTypes
import           Control.Monad.Except
import           Data.Char                      ( toLower )

eval :: LispVal -> ThrowsError LispVal
eval val@(String    _                  ) = return val
eval (    Atom      content            ) = return $ Atom $ map toLower content
eval val@(Number    _                  ) = return val
eval val@(Float     _                  ) = return val
eval val@(Character _                  ) = return val
eval val@(Bool      _                  ) = return val
eval val@(List      []                 ) = return val
eval (    List      [Atom "quote", val]) = return val
eval (    List      (Atom func : args) ) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe
  (throwError $ NotFunction "Unrecognized function args" func)
  ($ args)
  (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
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

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n  ) = return n
unpackNum (List   [n]) = unpackNum n
unpackNum notNum       = throwError $ TypeMismatch "number" notNum

isSymbol :: [LispVal] -> ThrowsError LispVal
isSymbol []                    = throwError $ NumArgs 1 []
isSymbol [(        Atom _)   ] = return $ Bool True
isSymbol ((Atom _) :     rest) = isSymbol rest
isSymbol (_        :     _   ) = return $ Bool False

isNumber :: [LispVal] -> ThrowsError LispVal
isNumber []                        = throwError $ NumArgs 1 []
isNumber [(          Number _)   ] = return $ Bool True
isNumber [(          Float  _)   ] = return $ Bool True
isNumber ((Number _) :       rest) = isNumber rest
isNumber ((Float  _) :       rest) = isNumber rest
isNumber (_          :       _   ) = return $ Bool False

isString :: [LispVal] -> ThrowsError LispVal
isString []                        = throwError $ NumArgs 1 []
isString [(          String _)   ] = return $ Bool True
isString ((String _) :       rest) = isString rest
isString (_          :       _   ) = return $ Bool False

isCharacter :: [LispVal] -> ThrowsError LispVal
isCharacter []                              = throwError $ NumArgs 1 []
isCharacter [(             Character _)   ] = return $ Bool True
isCharacter ((Character _) :          rest) = isCharacter rest
isCharacter (_             :          _   ) = return $ Bool False

isBool :: [LispVal] -> ThrowsError LispVal
isBool []                    = throwError $ NumArgs 1 []
isBool [(        Bool _)   ] = return $ Bool True
isBool ((Bool _) :     rest) = isBool rest
isBool (_        :     _   ) = return $ Bool False

stringToSymbol :: [LispVal] -> ThrowsError LispVal
stringToSymbol [(String val)] = return $ Atom $ map toLower val
stringToSymbol [nonStringVal] = throwError $ TypeMismatch "string" nonStringVal
stringToSymbol vals           = throwError $ NumArgs 1 vals

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [(Atom val)] = return $ String val
symbolToString [nonAtomVal] = throwError $ TypeMismatch "symbol" nonAtomVal
symbolToString vals         = throwError $ NumArgs 1 vals
