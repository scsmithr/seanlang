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

parseExpr :: P.Parser LispVal
parseExpr = parseAtom <|> parseString <|> parseNumber

symbol :: P.Parser Char
symbol = P.oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: P.Parser ()
spaces = P.skipMany1 P.space

readExpr :: String -> String
readExpr input = case P.parse parseExpr "lisp" input of
  Left  err -> "No match: " ++ show err
  Right _   -> "Found value"

