-- | Parser module

module HaskellScheme.Parser where
import           Control.Monad
import           Control.Monad.Except
import           HaskellScheme.LispTypes
import           Numeric
import           Text.ParserCombinators.Parsec
                                         hiding ( spaces )

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>@^_~?"

spaces :: Parser ()
spaces = skipMany1 space

parseEscapedChar :: Parser Char
parseEscapedChar = oneOf "\"nrt\\"

parseStringChar :: Parser Char
parseStringChar = noneOf "\"\\" <|> (char '\\' >> parseEscapedChar)

parseCharLiteral :: Parser LispVal
parseCharLiteral = do
  _ <- try $ string "#\\"
  charLit <- try (string "newline" <|> string "space") <|> do
    x <- anyChar
    _ <- notFollowedBy alphaNum
    return [x]
  return $ Character $ case charLit of
    "space"   -> ' '
    "newline" -> '\n'
    ""        -> ' '
    _         -> (charLit !! 0)

parseString :: Parser LispVal
parseString = do
  _ <- char '"'
  x <- many parseStringChar
  _ <- char '"'
  return $ String x

parseBool :: Parser LispVal
parseBool = do
  _ <- char '#'
  (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest  <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

-- TODO : Parse floats / decimal
-- TODO : Parse entire numerical tower
parseNumber :: Parser LispVal
parseNumber =
  ((many1 digit) >>= (return . Number . read))
    <|> parseDecimalAlt
    <|> parseHex
    <|> parseOct
    <|> parseBin

parseDecimalAlt :: Parser LispVal
parseDecimalAlt = do
  _ <- try $ string "#d"
  x <- many1 digit
  (return . Number . read) x

parseHex :: Parser LispVal
parseHex = do
  _ <- try $ string "#x"
  x <- many1 hexDigit
  return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do
  _ <- try $ string "#o"
  x <- many1 octDigit
  return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do
  _ <- try $ string "#b"
  x <- many1 (oneOf "10")
  return $ Number (bin2dig x)

oct2dig :: String -> Integer
oct2dig x = fst $ readOct x !! 0

hex2dig :: String -> Integer
hex2dig x = fst $ readHex x !! 0

bin2dig :: String -> Integer
bin2dig = bin2dig' 0
bin2dig' :: Num t => t -> [Char] -> t
bin2dig' digint "" = digint
bin2dig' digint (x : xs) =
  let old = 2 * digint + (if x == '0' then 0 else 1) in bin2dig' old xs

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
    <|> try parseNumber
    <|> try parseCharLiteral
    <|> try parseBool
    <|> parseQuoted
    <|> parseUnquote
    <|> parseQuasiquoted
    <|> do
          _ <- char '('
          x <- try parseList <|> parseDottedList
          _ <- char ')'
          return x


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left  err -> throwError $ Parser err
  Right val -> return val
