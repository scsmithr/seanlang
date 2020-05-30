module Parser
  ( readExpr
  )
where

import           Control.Monad
import           Text.ParserCombinators.Parsec  ( (<|>) )
import qualified Text.ParserCombinators.Parsec as P

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
  show (String contents) = "\"" ++ contents ++ "\""
  show (Atom name) = name
  show (Number contents) = show contents
  show (Bool True) = "#t"
  show (Bool False) = "#f"
  show (List contents) = "(" ++ unwordsList contents ++ ")"
  show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

parseString :: P.Parser LispVal
parseString = do
  _ <- P.char '"'
  x <- P.many (P.noneOf "\"")
  _ <- P.char '"'
  return $ String x

parseAtom :: P.Parser LispVal
parseAtom = do
  first <- P.letter <|> symbol
  rest  <- P.many (P.letter <|> P.digit <|> symbol)
  let atom = first : rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

parseNumber :: P.Parser LispVal
parseNumber = P.many1 P.digit >>= return . Number . read

parseList :: P.Parser LispVal
parseList = P.sepBy parseExpr spaces >>= return . List

parseDottedList :: P.Parser LispVal
parseDottedList = do
  head <- P.endBy parseExpr spaces
  tail <- P.char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: P.Parser LispVal
parseQuoted = do
  _ <- P.char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseSexp :: P.Parser LispVal
parseSexp = do
  _ <- P.char '('
  x <- P.try parseList <|> parseDottedList
  _ <- P.char ')'
  return x

parseExpr :: P.Parser LispVal
parseExpr =
  parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> parseSexp

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: P.Parser ()
spaces = P.skipMany1 P.space

readExpr :: String -> String
readExpr input = case P.parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right val -> "Found: " ++ show val

