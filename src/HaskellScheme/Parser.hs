-- | Parser module

module HaskellScheme.Parser where
import           Control.Monad
import           HaskellScheme.LispTypes
import           Numeric
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseEscapedChar :: Parser Char
parseEscapedChar = oneOf "\"nrt\\"

parseStringChar :: Parser Char
parseStringChar = noneOf "\"\\" <|> (char '\\' >> parseEscapedChar)

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
  _       <- string "#\\"
  charLit <- many (noneOf " ")
  return $ case charLit of
    "space"   -> Character ' '
    "newline" -> Character '\n'
    ""        -> Character ' '
    _         -> Character (charLit !! 0)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many parseStringChar
  _ <- char '"'
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

-- TODO : Parse floats / decimal
-- TODO : Parse entire numerical tower
parseNumber :: Parser LispVal
parseNumber =
  ((many1 digit) >>= (return . Number . read))
    <|> (   (string "#o")
        >>  (many1 digit)
        >>= (return . Number . fst . (!! 0) . readOct)
        )
    <|> (   (string "#x")
        >>  (many1 digit)
        >>= (return . Number . fst . (!! 0) . readHex)
        )

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  listHead <- endBy parseExpr spaces
  listTail <- char '.' >> spaces >> parseExpr
  return $ DottedList listHead listTail

parseQuoted :: Parser LispVal
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseQuasiquoted :: Parser LispVal
parseQuasiquoted = do
  _ <- char '`'
  x <- parseExpr
  return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do
  _ <- char ','
  x <- parseExpr
  return $ List [Atom "unquote", x]


parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseString
    <|> parseNumber
    <|> parseCharLiteral
    <|> parseQuoted
    <|> parseUnquote
    <|> parseQuasiquoted
    <|> do
          _ <- char '('
          x <- try parseList <|> parseDottedList
          _ <- char ')'
          return x


readExpr :: String -> Either String LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> Left $ "No match: " ++ show err
  Right val -> Right val
