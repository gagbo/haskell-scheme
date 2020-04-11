-- | Evaluation module

module HaskellScheme.Evaluator where
import           HaskellScheme.LispTypes

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
