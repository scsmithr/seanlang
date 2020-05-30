module Parser
  ( readExpr
  , eval
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

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom   name    ) = name
showVal (Number contents) = show contents
showVal (Bool   True    ) = "#t"
showVal (Bool   False   ) = "#f"
showVal (List   contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) =
  "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

instance Show LispVal where show = showVal

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser P.ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar     message var ) = message ++ ": " ++ var
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction    message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =
  "Expected " ++ show expected ++ " args, found values" ++ unwordsList found
showError (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error: " ++ show parseError

instance Show LispError where show = showError

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

eval :: LispVal -> Either LispError LispVal
eval val@(String _                  ) = return val
eval val@(Number _                  ) = return val
eval val@(Bool   _                  ) = return val
eval (    List   [Atom "quote", val]) = return val
eval (    List   (Atom func : args) ) = mapM eval args >>= apply func
eval badForm = Left $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> Either LispError LispVal
apply func args =
  maybe (Left $ NotFunction "Unrecognized primitive function" func) ($ args)
    $ lookup func primitives

primitives :: [(String, [LispVal] -> Either LispError LispVal)]
primitives =
  [ ("+"        , numericBinop (+))
  , ("-"        , numericBinop (-))
  , ("*"        , numericBinop (*))
  , ("/"        , numericBinop div)
  , ("mod"      , numericBinop mod)
  , ("quotient" , numericBinop quot)
  , ("remainder", numericBinop rem)
  ]

numericBinop
  :: (Integer -> Integer -> Integer) -> [LispVal] -> Either LispError LispVal
numericBinop op [] = Left $ NumArgs 2 []
numericBinop op singleVal@[_] = Left $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> Either LispError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n :: [(Integer, String)]
  in  if null parsed
        then Left $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = Left $ TypeMismatch "number" notNum

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

readExpr :: String -> Either LispError LispVal
readExpr input = case P.parse parseExpr "lisp" input of
  Left  err -> Left $ Parser err
  Right val -> Right val

