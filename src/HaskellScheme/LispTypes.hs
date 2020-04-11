-- | LispTypes

module HaskellScheme.LispTypes where
import           Text.ParserCombinators.Parsec  ( ParseError )
import           Control.Monad.Except

data LispVal = Atom String
 | List [LispVal]
 | DottedList [LispVal] LispVal
 | Number Integer
 | String String
 | Bool Bool
 | Character Char
 | Float Float

data LispError = NumArgs Integer [LispVal]
 | TypeMismatch String LispVal
 | Parser ParseError
 | BadSpecialForm String LispVal
 | NotFunction String String
 | UnboundVar String String
 | Default String

showError :: LispError -> String
showError (UnboundVar     message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form   ) = message ++ ": " ++ show form
showError (NotFunction    message func   ) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser  parseErr) = "Parse error at " ++ show parseErr
showError (Default err     ) = "error : " ++ show err

instance Show LispError where
  show = showError

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

type ThrowsError = Either LispError

trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
