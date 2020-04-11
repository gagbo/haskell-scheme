-- | LispTypes

module HaskellScheme.LispTypes where

data LispVal = Atom String
 | List [LispVal]
 | DottedList [LispVal] LispVal
 | Number Integer
 | String String
 | Bool Bool
 | Character Char
 | Float Float
