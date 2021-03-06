{-# LANGUAGE ExistentialQuantification #-}
-- | Evaluation module

module HaskellScheme.Evaluator where
import           HaskellScheme.LispTypes
import           Control.Monad.Except
import           Data.Char                      ( toLower )

eval :: LispVal -> ThrowsError LispVal
eval val@(String    _                               ) = return val
eval (Atom content) = return $ Atom $ map toLower content
eval val@(Number    _                               ) = return val
eval val@(Float     _                               ) = return val
eval val@(Character _                               ) = return val
eval val@(Bool      _                               ) = return val
eval val@(List      []                              ) = return val
eval (    List      [Atom "quote", val]             ) = return val
eval (    List      [Atom "if", ifPred, conseq, alt]) = do
  result <- eval ifPred
  case result of
    Bool False -> eval alt
    _          -> eval conseq
eval (List (Atom func : args)) = mapM eval args >>= apply func
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
  , ("eq?"           , eqv)
  , ("eqv?"          , eqv)
  , ("equal?"        , equal)
  , ("car"           , car)
  , ("cdr"           , cdr)
  , ("cons"          , cons)
  , ("="             , numBoolBinop (==))
  , ("<"             , numBoolBinop (<))
  , (">"             , numBoolBinop (>))
  , ("/="            , numBoolBinop (/=))
  , (">="            , numBoolBinop (>=))
  , ("<="            , numBoolBinop (<=))
  , ("&&"            , boolBoolBinop (&&))
  , ("||"            , boolBoolBinop (||))
  , ("string=?"      , strBoolBinop (==))
  , ("string<?"      , strBoolBinop (<))
  , ("string>?"      , strBoolBinop (>))
  , ("string<=?"     , strBoolBinop (<=))
  , ("string>=?"     , strBoolBinop (>=))
  ]

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ [] = throwError $ NumArgs 2 []
numericBinop _ singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
      unpacked1 <- unpacker arg1
      unpacked2 <- unpacker arg2
      return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

boolBinop
  :: (LispVal -> ThrowsError a)
  -> (a -> a -> Bool)
  -> [LispVal]
  -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
  then throwError $ NumArgs 2 args
  else do
    left  <- unpacker $ args !! 0
    right <- unpacker $ args !! 1
    return $ Bool $ left `op` right

numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n  ) = return n
unpackNum (List   [n]) = unpackNum n
unpackNum notNum       = throwError $ TypeMismatch "number" notNum

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool   s) = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

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

car :: [LispVal] -> ThrowsError LispVal
car [List (x : _)        ] = return x
car [DottedList (x : _) _] = return x
car [badArg              ] = throwError $ TypeMismatch "pair" badArg
car badArgList             = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_ : xs)        ] = return $ List xs
cdr [DottedList [_     ] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg               ] = throwError $ TypeMismatch "pair" badArg
cdr badArgList              = throwError $ NumArgs 1 badArgList

cons :: [LispVal] -> ThrowsError LispVal
cons [x1, List []            ] = return $ List [x1]
cons [x , List xs            ] = return $ List $ x : xs
cons [x , DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2                 ] = return $ DottedList [x1] x2
cons badArgList                = throwError $ NumArgs 2 badArgList

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool      arg1), (Bool arg2)     ] = return $ Bool $ arg1 == arg2
eqv [(Number    arg1), (Number arg2)   ] = return $ Bool $ arg1 == arg2
eqv [(String    arg1), (String arg2)   ] = return $ Bool $ arg1 == arg2
eqv [(Character arg1), (Character arg2)] = return $ Bool $ arg1 == arg2
eqv [(Float     arg1), (Float arg2)    ] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] =
  return $ Bool $ (map toLower arg1) == (map toLower arg2)
eqv [(DottedList xs x), (DottedList ys y)] =
  eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] =
  return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
 where
  eqvPair (x1, x2) = case eqv [x1, x2] of
    Left  _          -> False
    Right (Bool val) -> val
    Right _          -> False  -- Unreachable theoretically
eqv [_, _]     = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

equal :: [LispVal] -> ThrowsError LispVal
equal [(List argList1), (List argList2)] =
  return
    $  Bool
    $  (length argList1 == length argList2)
    && (all equalPair $ zip argList1 argList2)
 where
  equalPair (x1, x2) = case equal [x1, x2] of
    Left  _          -> False
    Right (Bool val) -> val
    Right _          -> False  -- Unreachable theoretically
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM
    (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

-- TODO : cond
-- TODO : case
-- TODO : string functions
